------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             P A R . L A B L                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2003, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
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
   --  Find the innermost body or block that encloses N.

   function Find_Enclosing_Body (N : Node_Id) return Node_Id;
   --  Find the innermost body that encloses N.

   procedure Check_Distinct_Labels;
   --  Checks the rule in RM-5.1(11), which requires distinct identifiers
   --  for all the labels in a given body.

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
      --  Climb up the parent chain until we find a body or block.

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

         if not Present (Declarations (Enclosing_Body_Or_Block)) then
            Set_Declarations (Enclosing_Body_Or_Block, New_List);
         end if;

         Append (Label_Decl_Node, Declarations (Enclosing_Body_Or_Block));
      end if;

      <<Next_Label>>
         Next_Elmt (Next_Label_Elmt);
   end loop;

end Labl;
