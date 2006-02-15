------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             P A R . L A B L                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2006, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

separate (Par)
procedure Labl is
   Enclosing_Body_Or_Block : Node_Id;
   --  Innermost enclosing body or block statement

   Label_Decl_Node : Node_Id;
   --  Implicit label declaration node

   Defining_Ident_Node : Node_Id;
   --  Defining identifier node for implicit label declaration

   Next_Label_Elmt : Elmt_Id;
   --  Next element on label element list

   Label_Node : Node_Id;
   --  Next label node to process

   function Find_Enclosing_Body_Or_Block (N : Node_Id) return Node_Id;
   --  Find the innermost body or block that encloses N

   function Find_Enclosing_Body (N : Node_Id) return Node_Id;
   --  Find the innermost body that encloses N

   procedure Check_Distinct_Labels;
   --  Checks the rule in RM-5.1(11), which requires distinct identifiers
   --  for all the labels in a given body.

   procedure Find_Natural_Loops;
   --  Recognizes loops created by backward gotos, and rewrites the
   --  corresponding statements into a proper loop, for optimization
   --  purposes (for example, to control reclaiming local storage).

   ---------------------------
   -- Check_Distinct_Labels --
   ---------------------------

   procedure Check_Distinct_Labels is
      Label_Id : constant Node_Id := Identifier (Label_Node);

      Enclosing_Body : constant Node_Id :=
                         Find_Enclosing_Body (Enclosing_Body_Or_Block);
      --  Innermost enclosing body

      Next_Other_Label_Elmt : Elmt_Id := First_Elmt (Label_List);
      --  Next element on label element list

      Other_Label : Node_Id;
      --  Next label node to process

   begin
      --  Loop through all the labels, and if we find some other label
      --  (i.e. not Label_Node) that has the same identifier,
      --  and whose innermost enclosing body is the same,
      --  then we have an error.

      --  Note that in the worst case, this is quadratic in the number
      --  of labels.  However, labels are not all that common, and this
      --  is only called for explicit labels.
      --  ???Nonetheless, the efficiency could be improved. For example,
      --  call Labl for each body, rather than once per compilation.

      while Present (Next_Other_Label_Elmt) loop
         Other_Label := Node (Next_Other_Label_Elmt);

         exit when Label_Node = Other_Label;

         if Chars (Label_Id) = Chars (Identifier (Other_Label))
           and then Enclosing_Body = Find_Enclosing_Body (Other_Label)
         then
            Error_Msg_Sloc := Sloc (Other_Label);
            Error_Msg_N ("& conflicts with label#", Label_Id);
            exit;
         end if;

         Next_Elmt (Next_Other_Label_Elmt);
      end loop;
   end Check_Distinct_Labels;

   -------------------------
   -- Find_Enclosing_Body --
   -------------------------

   function Find_Enclosing_Body (N : Node_Id) return Node_Id is
      Result : Node_Id := N;

   begin
      --  This is the same as Find_Enclosing_Body_Or_Block, except
      --  that we skip block statements and accept statements, instead
      --  of stopping at them.

      while Present (Result)
        and then Nkind (Result) /= N_Entry_Body
        and then Nkind (Result) /= N_Task_Body
        and then Nkind (Result) /= N_Package_Body
        and then Nkind (Result) /= N_Subprogram_Body
      loop
         Result := Parent (Result);
      end loop;

      return Result;
   end Find_Enclosing_Body;

   ----------------------------------
   -- Find_Enclosing_Body_Or_Block --
   ----------------------------------

   function Find_Enclosing_Body_Or_Block (N : Node_Id) return Node_Id is
      Result : Node_Id := Parent (N);

   begin
      --  Climb up the parent chain until we find a body or block

      while Present (Result)
        and then Nkind (Result) /= N_Accept_Statement
        and then Nkind (Result) /= N_Entry_Body
        and then Nkind (Result) /= N_Task_Body
        and then Nkind (Result) /= N_Package_Body
        and then Nkind (Result) /= N_Subprogram_Body
        and then Nkind (Result) /= N_Block_Statement
      loop
         Result := Parent (Result);
      end loop;

      return Result;
   end Find_Enclosing_Body_Or_Block;

   ------------------------
   -- Find_Natural_Loops --
   ------------------------

   procedure Find_Natural_Loops is
      Node_List : constant Elist_Id := New_Elmt_List;
      N         : Elmt_Id;
      Succ      : Elmt_Id;

      function Goto_Id (Goto_Node : Node_Id) return Name_Id;
      --  Find Name_Id of goto statement, which may be an expanded name

      function Matches
        (Label_Node : Node_Id;
         Goto_Node  : Node_Id) return Boolean;
      --  A label and a goto are candidates for a loop if the names match,
      --  and both nodes appear in the same body. In addition, both must
      --  appear in the same statement list. If they are not in the same
      --  statement list, the goto is from within an nested structure, and
      --  the label is not a header. We ignore the case where the goto is
      --  within a conditional structure, and capture only infinite loops.

      procedure Merge;
      --  Merge labels and goto statements in order of increasing sloc value.
      --  Discard labels of loop and block statements.

      procedure No_Header (N : Elmt_Id);
      --  The label N is known not to be a loop header. Scan forward and
      --  remove all subsequent goto's that may have this node as a target.

      procedure Process_Goto (N : Elmt_Id);
      --  N is a forward jump. Scan forward and remove all subsequent goto's
      --  that may have the same target, to preclude spurious loops.

      procedure Rewrite_As_Loop
        (Loop_Header : Node_Id;
         Loop_End    : Node_Id);
      --  Given a label and a backwards goto, rewrite intervening statements
      --  as a loop. Remove the label from the node list, and rewrite the
      --  goto with the body of the new loop.

      procedure Try_Loop (N : Elmt_Id);
      --  N is a label that may be a loop header. Scan forward to find some
      --  backwards goto with which to make a loop. Do nothing if there is
      --  an intervening label that is not part of a loop, or more than one
      --  goto with this target.

      -------------
      -- Goto_Id --
      -------------

      function Goto_Id (Goto_Node : Node_Id) return Name_Id is
      begin
         if Nkind (Name (Goto_Node)) = N_Identifier then
            return Chars (Name (Goto_Node));

         elsif Nkind (Name (Goto_Node)) = N_Selected_Component then
            return Chars (Selector_Name (Name (Goto_Node)));
         else

            --  In case of error, return Id that can't match anything

            return Name_Null;
         end if;
      end Goto_Id;

      -------------
      -- Matches --
      -------------

      function Matches
        (Label_Node : Node_Id;
         Goto_Node  :  Node_Id) return Boolean
      is
      begin
         return Chars (Identifier (Label_Node)) = Goto_Id (Goto_Node)
           and then Find_Enclosing_Body (Label_Node) =
                    Find_Enclosing_Body (Goto_Node);
      end Matches;

      -----------
      -- Merge --
      -----------

      procedure Merge is
         L1 : Elmt_Id;
         G1 : Elmt_Id;

      begin
         L1 := First_Elmt (Label_List);
         G1 := First_Elmt (Goto_List);

         while Present (L1)
           and then Present (G1)
         loop
            if Sloc (Node (L1)) < Sloc (Node (G1)) then

               --  Optimization: remove labels of loops and blocks, which
               --  play no role in what follows.

               if Nkind (Node (L1)) /= N_Loop_Statement
                 and then Nkind (Node (L1)) /= N_Block_Statement
               then
                  Append_Elmt (Node (L1), Node_List);
               end if;

               Next_Elmt (L1);

            else
               Append_Elmt (Node (G1), Node_List);
               Next_Elmt (G1);
            end if;
         end loop;

         while Present (L1) loop
            Append_Elmt (Node (L1), Node_List);
            Next_Elmt (L1);
         end loop;

         while Present (G1) loop
            Append_Elmt (Node (G1), Node_List);
            Next_Elmt (G1);
         end loop;
      end Merge;

      ---------------
      -- No_Header --
      ---------------

      procedure No_Header (N : Elmt_Id) is
         S1, S2 : Elmt_Id;

      begin
         S1 := Next_Elmt (N);
         while Present (S1) loop
            S2 := Next_Elmt (S1);
            if Nkind (Node (S1)) = N_Goto_Statement
              and then Matches (Node (N), Node (S1))
            then
               Remove_Elmt (Node_List, S1);
            end if;

            S1 := S2;
         end loop;
      end No_Header;

      ------------------
      -- Process_Goto --
      ------------------

      procedure Process_Goto (N : Elmt_Id) is
         Goto1 : constant Node_Id := Node (N);
         Goto2 : Node_Id;
         S, S1 : Elmt_Id;

      begin
         S := Next_Elmt (N);

         while Present (S) loop
            S1 := Next_Elmt (S);
            Goto2 := Node (S);

            if Nkind (Goto2) = N_Goto_Statement
              and then Goto_Id (Goto1) = Goto_Id (Goto2)
              and then Find_Enclosing_Body (Goto1) =
                       Find_Enclosing_Body (Goto2)
            then

               --  Goto2 may have the same target, remove it from
               --  consideration.

               Remove_Elmt (Node_List, S);
            end if;

            S := S1;
         end loop;
      end Process_Goto;

      ---------------------
      -- Rewrite_As_Loop --
      ---------------------

      procedure Rewrite_As_Loop
        (Loop_Header : Node_Id;
         Loop_End    : Node_Id)
      is
         Loop_Body : constant List_Id := New_List;
         Loop_Stmt : constant Node_Id :=
                       New_Node (N_Loop_Statement, Sloc (Loop_Header));
         Stat      : Node_Id;
         Next_Stat : Node_Id;
      begin
         Stat := Next (Loop_Header);
         while Stat /= Loop_End loop
            Next_Stat := Next (Stat);
            Remove (Stat);
            Append (Stat, Loop_Body);
            Stat := Next_Stat;
         end loop;

         Set_Statements (Loop_Stmt, Loop_Body);
         Set_Identifier (Loop_Stmt, Identifier (Loop_Header));

         Remove (Loop_Header);
         Rewrite (Loop_End, Loop_Stmt);
         Error_Msg_N
           ("code between label and backwards goto rewritten as loop?",
             Loop_End);
      end Rewrite_As_Loop;

      --------------
      -- Try_Loop --
      --------------

      procedure Try_Loop (N : Elmt_Id) is
         Source : Elmt_Id;
         Found  : Boolean := False;
         S1     : Elmt_Id;

      begin
         S1 := Next_Elmt (N);
         while Present (S1) loop
            if Nkind (Node (S1)) = N_Goto_Statement
              and then Matches (Node (N), Node (S1))
            then
               if not Found then
                  if Parent (Node (N)) = Parent (Node (S1)) then
                     Source := S1;
                     Found  := True;

                  else
                     --  The goto is within some nested structure

                     No_Header (N);
                     return;
                  end if;

               else
                  --  More than one goto with the same target

                  No_Header (N);
                  return;
               end if;

            elsif Nkind (Node (S1)) = N_Label
              and then not Found
            then
               --  Intervening label before possible end of loop. Current
               --  label is not a candidate. This is conservative, because
               --  the label might not be the target of any jumps, but not
               --  worth dealing with useless labels!

               No_Header (N);
               return;

            else
               --  If the node is a loop_statement, it corresponds to a
               --  label-goto pair rewritten as a loop. Continue forward scan.

               null;
            end if;

            Next_Elmt (S1);
         end loop;

         if Found then
            Rewrite_As_Loop (Node (N), Node (Source));
            Remove_Elmt (Node_List, N);
            Remove_Elmt (Node_List, Source);
         end if;
      end Try_Loop;

   begin
      --  Start of processing for Find_Natural_Loops

      Merge;

      N := First_Elmt (Node_List);
      while Present (N) loop
         Succ := Next_Elmt (N);

         if Nkind (Node (N)) = N_Label then
            if No (Succ) then
               exit;

            elsif Nkind (Node (Succ)) = N_Label then
               Try_Loop (Succ);

               --  If a loop was found, the label has been removed, and
               --  the following goto rewritten as the loop body.

               Succ := Next_Elmt (N);

               if Nkind (Node (Succ)) = N_Label then

                  --  Following label was not removed, so current label
                  --  is not a candidate header.

                  No_Header (N);

               else

                  --  Following label was part of inner loop. Current
                  --  label is still a candidate.

                  Try_Loop (N);
                  Succ := Next_Elmt (N);
               end if;

            elsif Nkind (Node (Succ)) = N_Goto_Statement then
               Try_Loop (N);
               Succ := Next_Elmt (N);
            end if;

         elsif Nkind (Node (N)) = N_Goto_Statement then
            Process_Goto (N);
            Succ := Next_Elmt (N);
         end if;

         N := Succ;
      end loop;
   end Find_Natural_Loops;

--  Start of processing for Par.Labl

begin
   Next_Label_Elmt := First_Elmt (Label_List);

   while Present (Next_Label_Elmt) loop
      Label_Node := Node (Next_Label_Elmt);

      if not Comes_From_Source (Label_Node) then
         goto Next_Label;
      end if;

      --  Find the innermost enclosing body or block, which is where
      --  we need to implicitly declare this label

      Enclosing_Body_Or_Block := Find_Enclosing_Body_Or_Block (Label_Node);

      --  If we didn't find a parent, then the label in question never got
      --  hooked into a reasonable declarative part. This happens only in
      --  error situations, and we simply ignore the entry (we aren't going
      --  to get into the semantics in any case given the error).

      if Present (Enclosing_Body_Or_Block) then
         Check_Distinct_Labels;

         --  Now create the implicit label declaration node and its
         --  corresponding defining identifier. Note that the defining
         --  occurrence of a label is the implicit label declaration that
         --  we are creating. The label itself is an applied occurrence.

         Label_Decl_Node :=
           New_Node (N_Implicit_Label_Declaration, Sloc (Label_Node));
         Defining_Ident_Node :=
           New_Entity (N_Defining_Identifier, Sloc (Identifier (Label_Node)));
         Set_Chars (Defining_Ident_Node, Chars (Identifier (Label_Node)));
         Set_Defining_Identifier (Label_Decl_Node, Defining_Ident_Node);
         Set_Label_Construct (Label_Decl_Node, Label_Node);

         --  The following makes sure that Comes_From_Source is appropriately
         --  set for the entity, depending on whether the label appeared in
         --  the source explicitly or not.

         Set_Comes_From_Source
          (Defining_Ident_Node, Comes_From_Source (Identifier (Label_Node)));

         --  Now attach the implicit label declaration to the appropriate
         --  declarative region, creating a declaration list if none exists

         if No (Declarations (Enclosing_Body_Or_Block)) then
            Set_Declarations (Enclosing_Body_Or_Block, New_List);
         end if;

         Append (Label_Decl_Node, Declarations (Enclosing_Body_Or_Block));
      end if;

      <<Next_Label>>
         Next_Elmt (Next_Label_Elmt);
   end loop;

   Find_Natural_Loops;

end Labl;
