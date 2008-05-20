------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              P R J . T R E E                             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2001-2008, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT; see file COPYING3.  If not, go to --
-- http://www.gnu.org/licenses for a complete copy of the license.          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Prj.Err;

package body Prj.Tree is

   Node_With_Comments : constant array (Project_Node_Kind) of Boolean :=
     (N_Project                    => True,
      N_With_Clause                => True,
      N_Project_Declaration        => False,
      N_Declarative_Item           => False,
      N_Package_Declaration        => True,
      N_String_Type_Declaration    => True,
      N_Literal_String             => False,
      N_Attribute_Declaration      => True,
      N_Typed_Variable_Declaration => True,
      N_Variable_Declaration       => True,
      N_Expression                 => False,
      N_Term                       => False,
      N_Literal_String_List        => False,
      N_Variable_Reference         => False,
      N_External_Value             => False,
      N_Attribute_Reference        => False,
      N_Case_Construction          => True,
      N_Case_Item                  => True,
      N_Comment_Zones              => True,
      N_Comment                    => True);
   --  Indicates the kinds of node that may have associated comments

   package Next_End_Nodes is new Table.Table
     (Table_Component_Type => Project_Node_Id,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 1,
      Table_Initial        => 10,
      Table_Increment      => 100,
      Table_Name           => "Next_End_Nodes");
   --  A stack of nodes to indicates to what node the next "end" is associated

   use Tree_Private_Part;

   End_Of_Line_Node   : Project_Node_Id := Empty_Node;
   --  The node an end of line comment may be associated with

   Previous_Line_Node : Project_Node_Id := Empty_Node;
   --  The node an immediately following comment may be associated with

   Previous_End_Node  : Project_Node_Id := Empty_Node;
   --  The node comments immediately following an "end" line may be
   --  associated with.

   Unkept_Comments    : Boolean := False;
   --  Set to True when some comments may not be associated with any node

   function Comment_Zones_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id;
   --  Returns the ID of the N_Comment_Zones node associated with node Node.
   --  If there is not already an N_Comment_Zones node, create one and
   --  associate it with node Node.

   ------------------
   -- Add_Comments --
   ------------------

   procedure Add_Comments
     (To       : Project_Node_Id;
      In_Tree  : Project_Node_Tree_Ref;
      Where    : Comment_Location) is
      Zone     : Project_Node_Id := Empty_Node;
      Previous : Project_Node_Id := Empty_Node;

   begin
      pragma Assert
        (Present (To)
          and then
         In_Tree.Project_Nodes.Table (To).Kind /= N_Comment);

      Zone := In_Tree.Project_Nodes.Table (To).Comments;

      if No (Zone) then

         --  Create new N_Comment_Zones node

         Project_Node_Table.Increment_Last (In_Tree.Project_Nodes);
         In_Tree.Project_Nodes.Table
           (Project_Node_Table.Last (In_Tree.Project_Nodes)) :=
           (Kind             => N_Comment_Zones,
            Qualifier        => Unspecified,
            Expr_Kind        => Undefined,
            Location         => No_Location,
            Directory        => No_Path,
            Variables        => Empty_Node,
            Packages         => Empty_Node,
            Pkg_Id           => Empty_Package,
            Name             => No_Name,
            Src_Index        => 0,
            Path_Name        => No_Path,
            Value            => No_Name,
            Field1           => Empty_Node,
            Field2           => Empty_Node,
            Field3           => Empty_Node,
            Field4           => Empty_Node,
            Flag1            => False,
            Flag2            => False,
            Comments         => Empty_Node);

         Zone := Project_Node_Table.Last (In_Tree.Project_Nodes);
         In_Tree.Project_Nodes.Table (To).Comments := Zone;
      end if;

      if Where = End_Of_Line then
         In_Tree.Project_Nodes.Table (Zone).Value := Comments.Table (1).Value;

      else
         --  Get each comments in the Comments table and link them to node To

         for J in 1 .. Comments.Last loop

            --  Create new N_Comment node

            if (Where = After or else Where = After_End) and then
              Token /= Tok_EOF and then
              Comments.Table (J).Follows_Empty_Line
            then
               Comments.Table (1 .. Comments.Last - J + 1) :=
                 Comments.Table (J .. Comments.Last);
               Comments.Set_Last (Comments.Last - J + 1);
               return;
            end if;

            Project_Node_Table.Increment_Last (In_Tree.Project_Nodes);
            In_Tree.Project_Nodes.Table
              (Project_Node_Table.Last (In_Tree.Project_Nodes)) :=
              (Kind             => N_Comment,
               Qualifier        => Unspecified,
               Expr_Kind        => Undefined,
               Flag1            => Comments.Table (J).Follows_Empty_Line,
               Flag2            =>
                 Comments.Table (J).Is_Followed_By_Empty_Line,
               Location         => No_Location,
               Directory        => No_Path,
               Variables        => Empty_Node,
               Packages         => Empty_Node,
               Pkg_Id           => Empty_Package,
               Name             => No_Name,
               Src_Index        => 0,
               Path_Name        => No_Path,
               Value            => Comments.Table (J).Value,
               Field1           => Empty_Node,
               Field2           => Empty_Node,
               Field3           => Empty_Node,
               Field4           => Empty_Node,
               Comments         => Empty_Node);

            --  If this is the first comment, put it in the right field of
            --  the node Zone.

            if No (Previous) then
               case Where is
                  when Before =>
                     In_Tree.Project_Nodes.Table (Zone).Field1 :=
                       Project_Node_Table.Last (In_Tree.Project_Nodes);

                  when After =>
                     In_Tree.Project_Nodes.Table (Zone).Field2 :=
                       Project_Node_Table.Last (In_Tree.Project_Nodes);

                  when Before_End =>
                     In_Tree.Project_Nodes.Table (Zone).Field3 :=
                       Project_Node_Table.Last (In_Tree.Project_Nodes);

                  when After_End =>
                     In_Tree.Project_Nodes.Table (Zone).Comments :=
                       Project_Node_Table.Last (In_Tree.Project_Nodes);

                  when End_Of_Line =>
                     null;
               end case;

            else
               --  When it is not the first, link it to the previous one

               In_Tree.Project_Nodes.Table (Previous).Comments :=
                 Project_Node_Table.Last (In_Tree.Project_Nodes);
            end if;

            --  This node becomes the previous one for the next comment, if
            --  there is one.

            Previous := Project_Node_Table.Last (In_Tree.Project_Nodes);
         end loop;
      end if;

      --  Empty the Comments table, so that there is no risk to link the same
      --  comments to another node.

      Comments.Set_Last (0);
   end Add_Comments;

   --------------------------------
   -- Associative_Array_Index_Of --
   --------------------------------

   function Associative_Array_Index_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Name_Id
   is
   begin
      pragma Assert
        (Present (Node)
          and then
            (In_Tree.Project_Nodes.Table (Node).Kind = N_Attribute_Declaration
               or else
             In_Tree.Project_Nodes.Table (Node).Kind = N_Attribute_Reference));
      return In_Tree.Project_Nodes.Table (Node).Value;
   end Associative_Array_Index_Of;

   ----------------------------
   -- Associative_Package_Of --
   ----------------------------

   function Associative_Package_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id
   is
   begin
      pragma Assert
        (Present (Node)
          and then
          (In_Tree.Project_Nodes.Table (Node).Kind = N_Attribute_Declaration));
      return In_Tree.Project_Nodes.Table (Node).Field3;
   end Associative_Package_Of;

   ----------------------------
   -- Associative_Project_Of --
   ----------------------------

   function Associative_Project_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id
   is
   begin
      pragma Assert
        (Present (Node)
          and then
          (In_Tree.Project_Nodes.Table (Node).Kind = N_Attribute_Declaration));
      return In_Tree.Project_Nodes.Table (Node).Field2;
   end Associative_Project_Of;

   ----------------------
   -- Case_Insensitive --
   ----------------------

   function Case_Insensitive
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Boolean is
   begin
      pragma Assert
        (Present (Node)
          and then
            (In_Tree.Project_Nodes.Table (Node).Kind = N_Attribute_Declaration
               or else
             In_Tree.Project_Nodes.Table (Node).Kind = N_Attribute_Reference));
      return In_Tree.Project_Nodes.Table (Node).Flag1;
   end Case_Insensitive;

   --------------------------------
   -- Case_Variable_Reference_Of --
   --------------------------------

   function Case_Variable_Reference_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id
   is
   begin
      pragma Assert
        (Present (Node)
          and then
            In_Tree.Project_Nodes.Table (Node).Kind = N_Case_Construction);
      return In_Tree.Project_Nodes.Table (Node).Field1;
   end Case_Variable_Reference_Of;

   ----------------------
   -- Comment_Zones_Of --
   ----------------------

   function Comment_Zones_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id
   is
      Zone : Project_Node_Id;

   begin
      pragma Assert (Present (Node));
      Zone := In_Tree.Project_Nodes.Table (Node).Comments;

      --  If there is not already an N_Comment_Zones associated, create a new
      --  one and associate it with node Node.

      if No (Zone) then
         Project_Node_Table.Increment_Last (In_Tree.Project_Nodes);
         Zone := Project_Node_Table.Last (In_Tree.Project_Nodes);
         In_Tree.Project_Nodes.Table (Zone) :=
        (Kind             => N_Comment_Zones,
         Qualifier        => Unspecified,
         Location         => No_Location,
         Directory        => No_Path,
         Expr_Kind        => Undefined,
         Variables        => Empty_Node,
         Packages         => Empty_Node,
         Pkg_Id           => Empty_Package,
         Name             => No_Name,
         Src_Index        => 0,
         Path_Name        => No_Path,
         Value            => No_Name,
         Field1           => Empty_Node,
         Field2           => Empty_Node,
         Field3           => Empty_Node,
         Field4           => Empty_Node,
         Flag1            => False,
         Flag2            => False,
         Comments         => Empty_Node);
         In_Tree.Project_Nodes.Table (Node).Comments := Zone;
      end if;

      return Zone;
   end Comment_Zones_Of;

   -----------------------
   -- Current_Item_Node --
   -----------------------

   function Current_Item_Node
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id
   is
   begin
      pragma Assert
        (Present (Node)
          and then
            In_Tree.Project_Nodes.Table (Node).Kind = N_Declarative_Item);
      return In_Tree.Project_Nodes.Table (Node).Field1;
   end Current_Item_Node;

   ------------------
   -- Current_Term --
   ------------------

   function Current_Term
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id
   is
   begin
      pragma Assert
        (Present (Node)
          and then
            In_Tree.Project_Nodes.Table (Node).Kind = N_Term);
      return In_Tree.Project_Nodes.Table (Node).Field1;
   end Current_Term;

   --------------------------
   -- Default_Project_Node --
   --------------------------

   function Default_Project_Node
     (In_Tree       : Project_Node_Tree_Ref;
      Of_Kind       : Project_Node_Kind;
      And_Expr_Kind : Variable_Kind := Undefined) return Project_Node_Id
   is
      Result   : Project_Node_Id;
      Zone     : Project_Node_Id;
      Previous : Project_Node_Id;

   begin
      --  Create new node with specified kind and expression kind

      Project_Node_Table.Increment_Last (In_Tree.Project_Nodes);
      In_Tree.Project_Nodes.Table
        (Project_Node_Table.Last (In_Tree.Project_Nodes)) :=
        (Kind             => Of_Kind,
         Qualifier        => Unspecified,
         Location         => No_Location,
         Directory        => No_Path,
         Expr_Kind        => And_Expr_Kind,
         Variables        => Empty_Node,
         Packages         => Empty_Node,
         Pkg_Id           => Empty_Package,
         Name             => No_Name,
         Src_Index        => 0,
         Path_Name        => No_Path,
         Value            => No_Name,
         Field1           => Empty_Node,
         Field2           => Empty_Node,
         Field3           => Empty_Node,
         Field4           => Empty_Node,
         Flag1            => False,
         Flag2            => False,
         Comments         => Empty_Node);

      --  Save the new node for the returned value

      Result := Project_Node_Table.Last (In_Tree.Project_Nodes);

      if Comments.Last > 0 then

         --  If this is not a node with comments, then set the flag

         if not Node_With_Comments (Of_Kind) then
            Unkept_Comments := True;

         elsif Of_Kind /= N_Comment and then Of_Kind /= N_Comment_Zones then

            Project_Node_Table.Increment_Last (In_Tree.Project_Nodes);
            In_Tree.Project_Nodes.Table
              (Project_Node_Table.Last (In_Tree.Project_Nodes)) :=
              (Kind             => N_Comment_Zones,
               Qualifier        => Unspecified,
               Expr_Kind        => Undefined,
               Location         => No_Location,
               Directory        => No_Path,
               Variables        => Empty_Node,
               Packages         => Empty_Node,
               Pkg_Id           => Empty_Package,
               Name             => No_Name,
               Src_Index        => 0,
               Path_Name        => No_Path,
               Value            => No_Name,
               Field1           => Empty_Node,
               Field2           => Empty_Node,
               Field3           => Empty_Node,
               Field4           => Empty_Node,
               Flag1            => False,
               Flag2            => False,
               Comments         => Empty_Node);

            Zone := Project_Node_Table.Last (In_Tree.Project_Nodes);
            In_Tree.Project_Nodes.Table (Result).Comments := Zone;
            Previous := Empty_Node;

            for J in 1 .. Comments.Last loop

               --  Create a new N_Comment node

               Project_Node_Table.Increment_Last (In_Tree.Project_Nodes);
               In_Tree.Project_Nodes.Table
                 (Project_Node_Table.Last (In_Tree.Project_Nodes)) :=
                 (Kind             => N_Comment,
                  Qualifier        => Unspecified,
                  Expr_Kind        => Undefined,
                  Flag1            => Comments.Table (J).Follows_Empty_Line,
                  Flag2            =>
                    Comments.Table (J).Is_Followed_By_Empty_Line,
                  Location         => No_Location,
                  Directory        => No_Path,
                  Variables        => Empty_Node,
                  Packages         => Empty_Node,
                  Pkg_Id           => Empty_Package,
                  Name             => No_Name,
                  Src_Index        => 0,
                  Path_Name        => No_Path,
                  Value            => Comments.Table (J).Value,
                  Field1           => Empty_Node,
                  Field2           => Empty_Node,
                  Field3           => Empty_Node,
                  Field4           => Empty_Node,
                  Comments         => Empty_Node);

               --  Link it to the N_Comment_Zones node, if it is the first,
               --  otherwise to the previous one.

               if No (Previous) then
                  In_Tree.Project_Nodes.Table (Zone).Field1 :=
                    Project_Node_Table.Last (In_Tree.Project_Nodes);

               else
                  In_Tree.Project_Nodes.Table (Previous).Comments :=
                    Project_Node_Table.Last (In_Tree.Project_Nodes);
               end if;

               --  This new node will be the previous one for the next
               --  N_Comment node, if there is one.

               Previous := Project_Node_Table.Last (In_Tree.Project_Nodes);
            end loop;

            --  Empty the Comments table after all comments have been processed

            Comments.Set_Last (0);
         end if;
      end if;

      return Result;
   end Default_Project_Node;

   ------------------
   -- Directory_Of --
   ------------------

   function Directory_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Path_Name_Type is
   begin
      pragma Assert
        (Present (Node)
          and then
            In_Tree.Project_Nodes.Table (Node).Kind = N_Project);
      return In_Tree.Project_Nodes.Table (Node).Directory;
   end Directory_Of;

   -------------------------
   -- End_Of_Line_Comment --
   -------------------------

   function End_Of_Line_Comment
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Name_Id is
      Zone : Project_Node_Id := Empty_Node;

   begin
      pragma Assert (Present (Node));
      Zone := In_Tree.Project_Nodes.Table (Node).Comments;

      if No (Zone) then
         return No_Name;
      else
         return In_Tree.Project_Nodes.Table (Zone).Value;
      end if;
   end End_Of_Line_Comment;

   ------------------------
   -- Expression_Kind_Of --
   ------------------------

   function Expression_Kind_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Variable_Kind is
   begin
      pragma Assert
        (Present (Node)
           and then
             (In_Tree.Project_Nodes.Table (Node).Kind = N_Literal_String
                or else
              In_Tree.Project_Nodes.Table (Node).Kind = N_Attribute_Declaration
                or else
              In_Tree.Project_Nodes.Table (Node).Kind = N_Variable_Declaration
                or else
              In_Tree.Project_Nodes.Table (Node).Kind =
                       N_Typed_Variable_Declaration
                or else
              In_Tree.Project_Nodes.Table (Node).Kind = N_Package_Declaration
                or else
              In_Tree.Project_Nodes.Table (Node).Kind = N_Expression
                or else
              In_Tree.Project_Nodes.Table (Node).Kind = N_Term
                or else
              In_Tree.Project_Nodes.Table (Node).Kind = N_Variable_Reference
                or else
              In_Tree.Project_Nodes.Table (Node).Kind =
                        N_Attribute_Reference));

      return In_Tree.Project_Nodes.Table (Node).Expr_Kind;
   end Expression_Kind_Of;

   -------------------
   -- Expression_Of --
   -------------------

   function Expression_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id
   is
   begin
      pragma Assert
        (Present (Node)
          and then
           (In_Tree.Project_Nodes.Table (Node).Kind =
              N_Attribute_Declaration
               or else
            In_Tree.Project_Nodes.Table (Node).Kind =
              N_Typed_Variable_Declaration
               or else
            In_Tree.Project_Nodes.Table (Node).Kind =
              N_Variable_Declaration));

      return In_Tree.Project_Nodes.Table (Node).Field1;
   end Expression_Of;

   -------------------------
   -- Extended_Project_Of --
   -------------------------

   function Extended_Project_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id
   is
   begin
      pragma Assert
        (Present (Node)
          and then
            In_Tree.Project_Nodes.Table (Node).Kind = N_Project_Declaration);
      return In_Tree.Project_Nodes.Table (Node).Field2;
   end Extended_Project_Of;

   ------------------------------
   -- Extended_Project_Path_Of --
   ------------------------------

   function Extended_Project_Path_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Path_Name_Type
   is
   begin
      pragma Assert
        (Present (Node)
          and then
            In_Tree.Project_Nodes.Table (Node).Kind = N_Project);
      return Path_Name_Type (In_Tree.Project_Nodes.Table (Node).Value);
   end Extended_Project_Path_Of;

   --------------------------
   -- Extending_Project_Of --
   --------------------------
   function Extending_Project_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id
   is
   begin
      pragma Assert
        (Present (Node)
          and then
            In_Tree.Project_Nodes.Table (Node).Kind = N_Project_Declaration);
      return In_Tree.Project_Nodes.Table (Node).Field3;
   end Extending_Project_Of;

   ---------------------------
   -- External_Reference_Of --
   ---------------------------

   function External_Reference_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id
   is
   begin
      pragma Assert
        (Present (Node)
          and then
            In_Tree.Project_Nodes.Table (Node).Kind = N_External_Value);
      return In_Tree.Project_Nodes.Table (Node).Field1;
   end External_Reference_Of;

   -------------------------
   -- External_Default_Of --
   -------------------------

   function External_Default_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref)
      return Project_Node_Id
   is
   begin
      pragma Assert
        (Present (Node)
          and then
            In_Tree.Project_Nodes.Table (Node).Kind = N_External_Value);
      return In_Tree.Project_Nodes.Table (Node).Field2;
   end External_Default_Of;

   ------------------------
   -- First_Case_Item_Of --
   ------------------------

   function First_Case_Item_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id
   is
   begin
      pragma Assert
        (Present (Node)
          and then
            In_Tree.Project_Nodes.Table (Node).Kind = N_Case_Construction);
      return In_Tree.Project_Nodes.Table (Node).Field2;
   end First_Case_Item_Of;

   ---------------------
   -- First_Choice_Of --
   ---------------------

   function First_Choice_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref)
      return Project_Node_Id
   is
   begin
      pragma Assert
        (Present (Node)
          and then
            In_Tree.Project_Nodes.Table (Node).Kind = N_Case_Item);
      return In_Tree.Project_Nodes.Table (Node).Field1;
   end First_Choice_Of;

   -------------------------
   -- First_Comment_After --
   -------------------------

   function First_Comment_After
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id
   is
      Zone : Project_Node_Id := Empty_Node;
   begin
      pragma Assert (Present (Node));
      Zone := In_Tree.Project_Nodes.Table (Node).Comments;

      if No (Zone) then
         return Empty_Node;

      else
         return In_Tree.Project_Nodes.Table (Zone).Field2;
      end if;
   end First_Comment_After;

   -----------------------------
   -- First_Comment_After_End --
   -----------------------------

   function First_Comment_After_End
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref)
      return Project_Node_Id
   is
      Zone : Project_Node_Id := Empty_Node;

   begin
      pragma Assert (Present (Node));
      Zone := In_Tree.Project_Nodes.Table (Node).Comments;

      if No (Zone) then
         return Empty_Node;

      else
         return In_Tree.Project_Nodes.Table (Zone).Comments;
      end if;
   end First_Comment_After_End;

   --------------------------
   -- First_Comment_Before --
   --------------------------

   function First_Comment_Before
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id
   is
      Zone : Project_Node_Id := Empty_Node;

   begin
      pragma Assert (Present (Node));
      Zone := In_Tree.Project_Nodes.Table (Node).Comments;

      if No (Zone) then
         return Empty_Node;

      else
         return In_Tree.Project_Nodes.Table (Zone).Field1;
      end if;
   end First_Comment_Before;

   ------------------------------
   -- First_Comment_Before_End --
   ------------------------------

   function First_Comment_Before_End
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id
   is
      Zone : Project_Node_Id := Empty_Node;

   begin
      pragma Assert (Present (Node));
      Zone := In_Tree.Project_Nodes.Table (Node).Comments;

      if No (Zone) then
         return Empty_Node;

      else
         return In_Tree.Project_Nodes.Table (Zone).Field3;
      end if;
   end First_Comment_Before_End;

   -------------------------------
   -- First_Declarative_Item_Of --
   -------------------------------

   function First_Declarative_Item_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id
   is
   begin
      pragma Assert
        (Present (Node)
          and then
            (In_Tree.Project_Nodes.Table (Node).Kind = N_Project_Declaration
               or else
             In_Tree.Project_Nodes.Table (Node).Kind = N_Case_Item
               or else
             In_Tree.Project_Nodes.Table (Node).Kind = N_Package_Declaration));

      if In_Tree.Project_Nodes.Table (Node).Kind = N_Project_Declaration then
         return In_Tree.Project_Nodes.Table (Node).Field1;
      else
         return In_Tree.Project_Nodes.Table (Node).Field2;
      end if;
   end First_Declarative_Item_Of;

   ------------------------------
   -- First_Expression_In_List --
   ------------------------------

   function First_Expression_In_List
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id
   is
   begin
      pragma Assert
        (Present (Node)
          and then
            In_Tree.Project_Nodes.Table (Node).Kind = N_Literal_String_List);
      return In_Tree.Project_Nodes.Table (Node).Field1;
   end First_Expression_In_List;

   --------------------------
   -- First_Literal_String --
   --------------------------

   function First_Literal_String
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id
   is
   begin
      pragma Assert
        (Present (Node)
          and then
         In_Tree.Project_Nodes.Table (Node).Kind =
           N_String_Type_Declaration);
      return In_Tree.Project_Nodes.Table (Node).Field1;
   end First_Literal_String;

   ----------------------
   -- First_Package_Of --
   ----------------------

   function First_Package_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Package_Declaration_Id
   is
   begin
      pragma Assert
        (Present (Node)
          and then
            In_Tree.Project_Nodes.Table (Node).Kind = N_Project);
      return In_Tree.Project_Nodes.Table (Node).Packages;
   end First_Package_Of;

   --------------------------
   -- First_String_Type_Of --
   --------------------------

   function First_String_Type_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id
   is
   begin
      pragma Assert
        (Present (Node)
          and then
            In_Tree.Project_Nodes.Table (Node).Kind = N_Project);
      return In_Tree.Project_Nodes.Table (Node).Field3;
   end First_String_Type_Of;

   ----------------
   -- First_Term --
   ----------------

   function First_Term
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id
   is
   begin
      pragma Assert
        (Present (Node)
          and then
            In_Tree.Project_Nodes.Table (Node).Kind = N_Expression);
      return In_Tree.Project_Nodes.Table (Node).Field1;
   end First_Term;

   -----------------------
   -- First_Variable_Of --
   -----------------------

   function First_Variable_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Variable_Node_Id
   is
   begin
      pragma Assert
        (Present (Node)
          and then
            (In_Tree.Project_Nodes.Table (Node).Kind = N_Project
               or else
             In_Tree.Project_Nodes.Table (Node).Kind = N_Package_Declaration));

      return In_Tree.Project_Nodes.Table (Node).Variables;
   end First_Variable_Of;

   --------------------------
   -- First_With_Clause_Of --
   --------------------------

   function First_With_Clause_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id
   is
   begin
      pragma Assert
        (Present (Node)
          and then
            In_Tree.Project_Nodes.Table (Node).Kind = N_Project);
      return In_Tree.Project_Nodes.Table (Node).Field1;
   end First_With_Clause_Of;

   ------------------------
   -- Follows_Empty_Line --
   ------------------------

   function Follows_Empty_Line
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Boolean is
   begin
      pragma Assert
        (Present (Node)
         and then
         In_Tree.Project_Nodes.Table (Node).Kind = N_Comment);
      return In_Tree.Project_Nodes.Table (Node).Flag1;
   end Follows_Empty_Line;

   ----------
   -- Hash --
   ----------

   function Hash (N : Project_Node_Id) return Header_Num is
   begin
      return Header_Num (N mod Project_Node_Id (Header_Num'Last));
   end Hash;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Tree : Project_Node_Tree_Ref) is
   begin
      Project_Node_Table.Init (Tree.Project_Nodes);
      Projects_Htable.Reset (Tree.Projects_HT);
   end Initialize;

   -------------------------------
   -- Is_Followed_By_Empty_Line --
   -------------------------------

   function Is_Followed_By_Empty_Line
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Boolean
   is
   begin
      pragma Assert
        (Present (Node)
          and then
            In_Tree.Project_Nodes.Table (Node).Kind = N_Comment);
      return In_Tree.Project_Nodes.Table (Node).Flag2;
   end Is_Followed_By_Empty_Line;

   ----------------------
   -- Is_Extending_All --
   ----------------------

   function Is_Extending_All
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Boolean is
   begin
      pragma Assert
        (Present (Node)
          and then
           (In_Tree.Project_Nodes.Table (Node).Kind = N_Project
              or else
            In_Tree.Project_Nodes.Table (Node).Kind = N_With_Clause));
      return In_Tree.Project_Nodes.Table (Node).Flag2;
   end Is_Extending_All;

   -------------------------
   -- Is_Not_Last_In_List --
   -------------------------

   function Is_Not_Last_In_List
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Boolean is
   begin
      pragma Assert
        (Present (Node)
          and then
            In_Tree.Project_Nodes.Table (Node).Kind = N_With_Clause);
      return In_Tree.Project_Nodes.Table (Node).Flag1;
   end Is_Not_Last_In_List;

   -------------------------------------
   -- Imported_Or_Extended_Project_Of --
   -------------------------------------

   function Imported_Or_Extended_Project_Of
     (Project   : Project_Node_Id;
      In_Tree   : Project_Node_Tree_Ref;
      With_Name : Name_Id) return Project_Node_Id
   is
      With_Clause : Project_Node_Id :=
        First_With_Clause_Of (Project, In_Tree);
      Result      : Project_Node_Id := Empty_Node;

   begin
      --  First check all the imported projects

      while Present (With_Clause) loop

         --  Only non limited imported project may be used as prefix
         --  of variable or attributes.

         Result := Non_Limited_Project_Node_Of (With_Clause, In_Tree);
         exit when Present (Result)
           and then Name_Of (Result, In_Tree) = With_Name;
         With_Clause := Next_With_Clause_Of (With_Clause, In_Tree);
      end loop;

      --  If it is not an imported project, it might be an extended project

      if No (With_Clause) then
         Result := Project;
         loop
            Result :=
              Extended_Project_Of
                (Project_Declaration_Of (Result, In_Tree), In_Tree);

            exit when No (Result)
              or else Name_Of (Result, In_Tree) = With_Name;
         end loop;
      end if;

      return Result;
   end Imported_Or_Extended_Project_Of;

   -------------
   -- Kind_Of --
   -------------

   function Kind_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Kind is
   begin
      pragma Assert (Present (Node));
      return In_Tree.Project_Nodes.Table (Node).Kind;
   end Kind_Of;

   -----------------
   -- Location_Of --
   -----------------

   function Location_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Source_Ptr is
   begin
      pragma Assert (Present (Node));
      return In_Tree.Project_Nodes.Table (Node).Location;
   end Location_Of;

   -------------
   -- Name_Of --
   -------------

   function Name_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Name_Id is
   begin
      pragma Assert (Present (Node));
      return In_Tree.Project_Nodes.Table (Node).Name;
   end Name_Of;

   --------------------
   -- Next_Case_Item --
   --------------------

   function Next_Case_Item
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id
   is
   begin
      pragma Assert
        (Present (Node)
          and then
            In_Tree.Project_Nodes.Table (Node).Kind = N_Case_Item);
      return In_Tree.Project_Nodes.Table (Node).Field3;
   end Next_Case_Item;

   ------------------
   -- Next_Comment --
   ------------------

   function Next_Comment
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id is
   begin
      pragma Assert
        (Present (Node)
          and then
            In_Tree.Project_Nodes.Table (Node).Kind = N_Comment);
      return In_Tree.Project_Nodes.Table (Node).Comments;
   end Next_Comment;

   ---------------------------
   -- Next_Declarative_Item --
   ---------------------------

   function Next_Declarative_Item
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id
   is
   begin
      pragma Assert
        (Present (Node)
          and then
            In_Tree.Project_Nodes.Table (Node).Kind = N_Declarative_Item);
      return In_Tree.Project_Nodes.Table (Node).Field2;
   end Next_Declarative_Item;

   -----------------------------
   -- Next_Expression_In_List --
   -----------------------------

   function Next_Expression_In_List
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id
   is
   begin
      pragma Assert
        (Present (Node)
          and then
            In_Tree.Project_Nodes.Table (Node).Kind = N_Expression);
      return In_Tree.Project_Nodes.Table (Node).Field2;
   end Next_Expression_In_List;

   -------------------------
   -- Next_Literal_String --
   -------------------------

   function Next_Literal_String
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref)
      return Project_Node_Id
   is
   begin
      pragma Assert
        (Present (Node)
          and then
            In_Tree.Project_Nodes.Table (Node).Kind = N_Literal_String);
      return In_Tree.Project_Nodes.Table (Node).Field1;
   end Next_Literal_String;

   -----------------------------
   -- Next_Package_In_Project --
   -----------------------------

   function Next_Package_In_Project
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id
   is
   begin
      pragma Assert
        (Present (Node)
          and then
            In_Tree.Project_Nodes.Table (Node).Kind = N_Package_Declaration);
      return In_Tree.Project_Nodes.Table (Node).Field3;
   end Next_Package_In_Project;

   ----------------------
   -- Next_String_Type --
   ----------------------

   function Next_String_Type
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref)
      return Project_Node_Id
   is
   begin
      pragma Assert
        (Present (Node)
          and then
         In_Tree.Project_Nodes.Table (Node).Kind =
           N_String_Type_Declaration);
      return In_Tree.Project_Nodes.Table (Node).Field2;
   end Next_String_Type;

   ---------------
   -- Next_Term --
   ---------------

   function Next_Term
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id
   is
   begin
      pragma Assert
        (Present (Node)
          and then
            In_Tree.Project_Nodes.Table (Node).Kind = N_Term);
      return In_Tree.Project_Nodes.Table (Node).Field2;
   end Next_Term;

   -------------------
   -- Next_Variable --
   -------------------

   function Next_Variable
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref)
      return Project_Node_Id
   is
   begin
      pragma Assert
        (Present (Node)
          and then
           (In_Tree.Project_Nodes.Table (Node).Kind =
              N_Typed_Variable_Declaration
               or else
            In_Tree.Project_Nodes.Table (Node).Kind =
              N_Variable_Declaration));

      return In_Tree.Project_Nodes.Table (Node).Field3;
   end Next_Variable;

   -------------------------
   -- Next_With_Clause_Of --
   -------------------------

   function Next_With_Clause_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id
   is
   begin
      pragma Assert
        (Present (Node)
          and then
            In_Tree.Project_Nodes.Table (Node).Kind = N_With_Clause);
      return In_Tree.Project_Nodes.Table (Node).Field2;
   end Next_With_Clause_Of;

   --------
   -- No --
   --------

   function No (Node : Project_Node_Id) return Boolean is
   begin
      return Node = Empty_Node;
   end No;

   ---------------------------------
   -- Non_Limited_Project_Node_Of --
   ---------------------------------

   function Non_Limited_Project_Node_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id
   is
   begin
      pragma Assert
        (Present (Node)
          and then
           (In_Tree.Project_Nodes.Table (Node).Kind = N_With_Clause));
      return In_Tree.Project_Nodes.Table (Node).Field3;
   end Non_Limited_Project_Node_Of;

   -------------------
   -- Package_Id_Of --
   -------------------

   function Package_Id_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Package_Node_Id
   is
   begin
      pragma Assert
        (Present (Node)
          and then
            In_Tree.Project_Nodes.Table (Node).Kind = N_Package_Declaration);
      return In_Tree.Project_Nodes.Table (Node).Pkg_Id;
   end Package_Id_Of;

   ---------------------
   -- Package_Node_Of --
   ---------------------

   function Package_Node_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id
   is
   begin
      pragma Assert
        (Present (Node)
          and then
            (In_Tree.Project_Nodes.Table (Node).Kind = N_Variable_Reference
               or else
             In_Tree.Project_Nodes.Table (Node).Kind = N_Attribute_Reference));
      return In_Tree.Project_Nodes.Table (Node).Field2;
   end Package_Node_Of;

   ------------------
   -- Path_Name_Of --
   ------------------

   function Path_Name_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Path_Name_Type
   is
   begin
      pragma Assert
        (Present (Node)
          and then
            (In_Tree.Project_Nodes.Table (Node).Kind = N_Project
               or else
             In_Tree.Project_Nodes.Table (Node).Kind = N_With_Clause));
      return In_Tree.Project_Nodes.Table (Node).Path_Name;
   end Path_Name_Of;

   -------------
   -- Present --
   -------------

   function Present (Node : Project_Node_Id) return Boolean is
   begin
      return Node /= Empty_Node;
   end Present;

   ----------------------------
   -- Project_Declaration_Of --
   ----------------------------

   function Project_Declaration_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id
   is
   begin
      pragma Assert
        (Present (Node)
          and then
            In_Tree.Project_Nodes.Table (Node).Kind = N_Project);
      return In_Tree.Project_Nodes.Table (Node).Field2;
   end Project_Declaration_Of;

   --------------------------
   -- Project_Qualifier_Of --
   --------------------------

   function Project_Qualifier_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Qualifier
   is
   begin
      pragma Assert
        (Present (Node)
          and then
            In_Tree.Project_Nodes.Table (Node).Kind = N_Project);
      return In_Tree.Project_Nodes.Table (Node).Qualifier;
   end Project_Qualifier_Of;

   -----------------------
   -- Parent_Project_Of --
   -----------------------

   function Parent_Project_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id
   is
   begin
      pragma Assert
        (Present (Node)
          and then
            In_Tree.Project_Nodes.Table (Node).Kind = N_Project);
      return In_Tree.Project_Nodes.Table (Node).Field4;
   end Parent_Project_Of;

   -------------------------------------------
   -- Project_File_Includes_Unkept_Comments --
   -------------------------------------------

   function Project_File_Includes_Unkept_Comments
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Boolean
   is
      Declaration : constant Project_Node_Id :=
                      Project_Declaration_Of (Node, In_Tree);
   begin
      return In_Tree.Project_Nodes.Table (Declaration).Flag1;
   end Project_File_Includes_Unkept_Comments;

   ---------------------
   -- Project_Node_Of --
   ---------------------

   function Project_Node_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id
   is
   begin
      pragma Assert
        (Present (Node)
          and then
           (In_Tree.Project_Nodes.Table (Node).Kind = N_With_Clause
              or else
            In_Tree.Project_Nodes.Table (Node).Kind = N_Variable_Reference
              or else
            In_Tree.Project_Nodes.Table (Node).Kind = N_Attribute_Reference));
      return In_Tree.Project_Nodes.Table (Node).Field1;
   end Project_Node_Of;

   -----------------------------------
   -- Project_Of_Renamed_Package_Of --
   -----------------------------------

   function Project_Of_Renamed_Package_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id
   is
   begin
      pragma Assert
        (Present (Node)
          and then
            In_Tree.Project_Nodes.Table (Node).Kind = N_Package_Declaration);
      return In_Tree.Project_Nodes.Table (Node).Field1;
   end Project_Of_Renamed_Package_Of;

   --------------------------
   -- Remove_Next_End_Node --
   --------------------------

   procedure Remove_Next_End_Node is
   begin
      Next_End_Nodes.Decrement_Last;
   end Remove_Next_End_Node;

   -----------------
   -- Reset_State --
   -----------------

   procedure Reset_State is
   begin
      End_Of_Line_Node   := Empty_Node;
      Previous_Line_Node := Empty_Node;
      Previous_End_Node  := Empty_Node;
      Unkept_Comments    := False;
      Comments.Set_Last (0);
   end Reset_State;

   -------------
   -- Restore --
   -------------

   procedure Restore (S : Comment_State) is
   begin
      End_Of_Line_Node   := S.End_Of_Line_Node;
      Previous_Line_Node := S.Previous_Line_Node;
      Previous_End_Node  := S.Previous_End_Node;
      Next_End_Nodes.Set_Last (0);
      Unkept_Comments    := S.Unkept_Comments;

      Comments.Set_Last (0);

      for J in S.Comments'Range loop
         Comments.Increment_Last;
         Comments.Table (Comments.Last) := S.Comments (J);
      end loop;
   end Restore;

   ----------
   -- Save --
   ----------

   procedure Save (S : out Comment_State) is
      Cmts : constant Comments_Ptr := new Comment_Array (1 .. Comments.Last);

   begin
      for J in 1 .. Comments.Last loop
         Cmts (J) := Comments.Table (J);
      end loop;

      S :=
        (End_Of_Line_Node   => End_Of_Line_Node,
         Previous_Line_Node => Previous_Line_Node,
         Previous_End_Node  => Previous_End_Node,
         Unkept_Comments    => Unkept_Comments,
         Comments           => Cmts);
   end Save;

   ----------
   -- Scan --
   ----------

   procedure Scan (In_Tree : Project_Node_Tree_Ref) is
      Empty_Line : Boolean := False;

   begin
      --  If there are comments, then they will not be kept. Set the flag and
      --  clear the comments.

      if Comments.Last > 0 then
         Unkept_Comments := True;
         Comments.Set_Last (0);
      end if;

      --  Loop until a token other that End_Of_Line or Comment is found

      loop
         Prj.Err.Scanner.Scan;

         case Token is
            when Tok_End_Of_Line =>
               if Prev_Token = Tok_End_Of_Line then
                  Empty_Line := True;

                  if Comments.Last > 0 then
                     Comments.Table (Comments.Last).Is_Followed_By_Empty_Line
                     := True;
                  end if;
               end if;

            when Tok_Comment =>
               --  If this is a line comment, add it to the comment table

               if Prev_Token = Tok_End_Of_Line
                 or else Prev_Token = No_Token
               then
                  Comments.Increment_Last;
                  Comments.Table (Comments.Last) :=
                    (Value                     => Comment_Id,
                     Follows_Empty_Line        => Empty_Line,
                     Is_Followed_By_Empty_Line => False);

               --  Otherwise, it is an end of line comment. If there is
               --  an end of line node specified, associate the comment with
               --  this node.

               elsif Present (End_Of_Line_Node) then
                  declare
                     Zones : constant Project_Node_Id :=
                               Comment_Zones_Of (End_Of_Line_Node, In_Tree);
                  begin
                     In_Tree.Project_Nodes.Table (Zones).Value := Comment_Id;
                  end;

               --  Otherwise, this end of line node cannot be kept

               else
                  Unkept_Comments := True;
                  Comments.Set_Last (0);
               end if;

               Empty_Line := False;

            when others =>
               --  If there are comments, where the first comment is not
               --  following an empty line, put the initial uninterrupted
               --  comment zone with the node of the preceding line (either
               --  a Previous_Line or a Previous_End node), if any.

               if Comments.Last > 0 and then
                 not Comments.Table (1).Follows_Empty_Line then
                  if Present (Previous_Line_Node) then
                     Add_Comments
                       (To      => Previous_Line_Node,
                        Where   => After,
                        In_Tree => In_Tree);

                  elsif Present (Previous_End_Node) then
                     Add_Comments
                       (To      => Previous_End_Node,
                        Where   => After_End,
                        In_Tree => In_Tree);
                  end if;
               end if;

               --  If there are still comments and the token is "end", then
               --  put these comments with the Next_End node, if any;
               --  otherwise, these comments cannot be kept. Always clear
               --  the comments.

               if Comments.Last > 0 and then Token = Tok_End then
                  if Next_End_Nodes.Last > 0 then
                     Add_Comments
                       (To      => Next_End_Nodes.Table (Next_End_Nodes.Last),
                        Where   => Before_End,
                        In_Tree => In_Tree);

                  else
                     Unkept_Comments := True;
                  end if;

                  Comments.Set_Last (0);
               end if;

               --  Reset the End_Of_Line, Previous_Line and Previous_End nodes
               --  so that they are not used again.

               End_Of_Line_Node   := Empty_Node;
               Previous_Line_Node := Empty_Node;
               Previous_End_Node  := Empty_Node;

               --  And return

               exit;
         end case;
      end loop;
   end Scan;

   ------------------------------------
   -- Set_Associative_Array_Index_Of --
   ------------------------------------

   procedure Set_Associative_Array_Index_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Name_Id)
   is
   begin
      pragma Assert
        (Present (Node)
          and then
            (In_Tree.Project_Nodes.Table (Node).Kind = N_Attribute_Declaration
               or else
             In_Tree.Project_Nodes.Table (Node).Kind = N_Attribute_Reference));
      In_Tree.Project_Nodes.Table (Node).Value := To;
   end Set_Associative_Array_Index_Of;

   --------------------------------
   -- Set_Associative_Package_Of --
   --------------------------------

   procedure Set_Associative_Package_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id)
   is
   begin
      pragma Assert
         (Present (Node)
          and then
            In_Tree.Project_Nodes.Table (Node).Kind = N_Attribute_Declaration);
      In_Tree.Project_Nodes.Table (Node).Field3 := To;
   end Set_Associative_Package_Of;

   --------------------------------
   -- Set_Associative_Project_Of --
   --------------------------------

   procedure Set_Associative_Project_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id)
   is
   begin
      pragma Assert
        (Present (Node)
          and then
           (In_Tree.Project_Nodes.Table (Node).Kind =
              N_Attribute_Declaration));
      In_Tree.Project_Nodes.Table (Node).Field2 := To;
   end Set_Associative_Project_Of;

   --------------------------
   -- Set_Case_Insensitive --
   --------------------------

   procedure Set_Case_Insensitive
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Boolean)
   is
   begin
      pragma Assert
        (Present (Node)
          and then
           (In_Tree.Project_Nodes.Table (Node).Kind = N_Attribute_Declaration
               or else
            In_Tree.Project_Nodes.Table (Node).Kind = N_Attribute_Reference));
      In_Tree.Project_Nodes.Table (Node).Flag1 := To;
   end Set_Case_Insensitive;

   ------------------------------------
   -- Set_Case_Variable_Reference_Of --
   ------------------------------------

   procedure Set_Case_Variable_Reference_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id)
   is
   begin
      pragma Assert
        (Present (Node)
          and then
            In_Tree.Project_Nodes.Table (Node).Kind = N_Case_Construction);
      In_Tree.Project_Nodes.Table (Node).Field1 := To;
   end Set_Case_Variable_Reference_Of;

   ---------------------------
   -- Set_Current_Item_Node --
   ---------------------------

   procedure Set_Current_Item_Node
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id)
   is
   begin
      pragma Assert
        (Present (Node)
          and then
            In_Tree.Project_Nodes.Table (Node).Kind = N_Declarative_Item);
      In_Tree.Project_Nodes.Table (Node).Field1 := To;
   end Set_Current_Item_Node;

   ----------------------
   -- Set_Current_Term --
   ----------------------

   procedure Set_Current_Term
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id)
   is
   begin
      pragma Assert
        (Present (Node)
          and then
            In_Tree.Project_Nodes.Table (Node).Kind = N_Term);
      In_Tree.Project_Nodes.Table (Node).Field1 := To;
   end Set_Current_Term;

   ----------------------
   -- Set_Directory_Of --
   ----------------------

   procedure Set_Directory_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Path_Name_Type)
   is
   begin
      pragma Assert
        (Present (Node)
          and then
            In_Tree.Project_Nodes.Table (Node).Kind = N_Project);
      In_Tree.Project_Nodes.Table (Node).Directory := To;
   end Set_Directory_Of;

   ---------------------
   -- Set_End_Of_Line --
   ---------------------

   procedure Set_End_Of_Line (To : Project_Node_Id) is
   begin
      End_Of_Line_Node := To;
   end Set_End_Of_Line;

   ----------------------------
   -- Set_Expression_Kind_Of --
   ----------------------------

   procedure Set_Expression_Kind_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Variable_Kind)
   is
   begin
      pragma Assert
        (Present (Node)
           and then
             (In_Tree.Project_Nodes.Table (Node).Kind = N_Literal_String
                or else
              In_Tree.Project_Nodes.Table (Node).Kind = N_Attribute_Declaration
                or else
              In_Tree.Project_Nodes.Table (Node).Kind = N_Variable_Declaration
                or else
              In_Tree.Project_Nodes.Table (Node).Kind =
                N_Typed_Variable_Declaration
                or else
              In_Tree.Project_Nodes.Table (Node).Kind = N_Package_Declaration
                or else
              In_Tree.Project_Nodes.Table (Node).Kind = N_Expression
                or else
              In_Tree.Project_Nodes.Table (Node).Kind = N_Term
                or else
              In_Tree.Project_Nodes.Table (Node).Kind = N_Variable_Reference
                or else
              In_Tree.Project_Nodes.Table (Node).Kind =
                N_Attribute_Reference));
      In_Tree.Project_Nodes.Table (Node).Expr_Kind := To;
   end Set_Expression_Kind_Of;

   -----------------------
   -- Set_Expression_Of --
   -----------------------

   procedure Set_Expression_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id)
   is
   begin
      pragma Assert
        (Present (Node)
          and then
           (In_Tree.Project_Nodes.Table (Node).Kind =
              N_Attribute_Declaration
               or else
            In_Tree.Project_Nodes.Table (Node).Kind =
              N_Typed_Variable_Declaration
               or else
            In_Tree.Project_Nodes.Table (Node).Kind =
              N_Variable_Declaration));
      In_Tree.Project_Nodes.Table (Node).Field1 := To;
   end Set_Expression_Of;

   -------------------------------
   -- Set_External_Reference_Of --
   -------------------------------

   procedure Set_External_Reference_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id)
   is
   begin
      pragma Assert
        (Present (Node)
          and then
            In_Tree.Project_Nodes.Table (Node).Kind = N_External_Value);
      In_Tree.Project_Nodes.Table (Node).Field1 := To;
   end Set_External_Reference_Of;

   -----------------------------
   -- Set_External_Default_Of --
   -----------------------------

   procedure Set_External_Default_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id)
   is
   begin
      pragma Assert
        (Present (Node)
          and then
            In_Tree.Project_Nodes.Table (Node).Kind = N_External_Value);
      In_Tree.Project_Nodes.Table (Node).Field2 := To;
   end Set_External_Default_Of;

   ----------------------------
   -- Set_First_Case_Item_Of --
   ----------------------------

   procedure Set_First_Case_Item_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id)
   is
   begin
      pragma Assert
        (Present (Node)
          and then
            In_Tree.Project_Nodes.Table (Node).Kind = N_Case_Construction);
      In_Tree.Project_Nodes.Table (Node).Field2 := To;
   end Set_First_Case_Item_Of;

   -------------------------
   -- Set_First_Choice_Of --
   -------------------------

   procedure Set_First_Choice_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id)
   is
   begin
      pragma Assert
        (Present (Node)
          and then
            In_Tree.Project_Nodes.Table (Node).Kind = N_Case_Item);
      In_Tree.Project_Nodes.Table (Node).Field1 := To;
   end Set_First_Choice_Of;

   -----------------------------
   -- Set_First_Comment_After --
   -----------------------------

   procedure Set_First_Comment_After
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id)
   is
      Zone : constant Project_Node_Id := Comment_Zones_Of (Node, In_Tree);
   begin
      In_Tree.Project_Nodes.Table (Zone).Field2 := To;
   end Set_First_Comment_After;

   ---------------------------------
   -- Set_First_Comment_After_End --
   ---------------------------------

   procedure Set_First_Comment_After_End
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id)
   is
      Zone : constant Project_Node_Id := Comment_Zones_Of (Node, In_Tree);
   begin
      In_Tree.Project_Nodes.Table (Zone).Comments := To;
   end Set_First_Comment_After_End;

   ------------------------------
   -- Set_First_Comment_Before --
   ------------------------------

   procedure Set_First_Comment_Before
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id)

   is
      Zone : constant Project_Node_Id := Comment_Zones_Of (Node, In_Tree);
   begin
      In_Tree.Project_Nodes.Table (Zone).Field1 := To;
   end Set_First_Comment_Before;

   ----------------------------------
   -- Set_First_Comment_Before_End --
   ----------------------------------

   procedure Set_First_Comment_Before_End
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id)
   is
      Zone : constant Project_Node_Id := Comment_Zones_Of (Node, In_Tree);
   begin
      In_Tree.Project_Nodes.Table (Zone).Field2 := To;
   end Set_First_Comment_Before_End;

   ------------------------
   -- Set_Next_Case_Item --
   ------------------------

   procedure Set_Next_Case_Item
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id)
   is
   begin
      pragma Assert
        (Present (Node)
          and then
            In_Tree.Project_Nodes.Table (Node).Kind = N_Case_Item);
      In_Tree.Project_Nodes.Table (Node).Field3 := To;
   end Set_Next_Case_Item;

   ----------------------
   -- Set_Next_Comment --
   ----------------------

   procedure Set_Next_Comment
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id)
   is
   begin
      pragma Assert
        (Present (Node)
          and then
            In_Tree.Project_Nodes.Table (Node).Kind = N_Comment);
      In_Tree.Project_Nodes.Table (Node).Comments := To;
   end Set_Next_Comment;

   -----------------------------------
   -- Set_First_Declarative_Item_Of --
   -----------------------------------

   procedure Set_First_Declarative_Item_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id)
   is
   begin
      pragma Assert
        (Present (Node)
          and then
            (In_Tree.Project_Nodes.Table (Node).Kind = N_Project_Declaration
               or else
             In_Tree.Project_Nodes.Table (Node).Kind = N_Case_Item
               or else
             In_Tree.Project_Nodes.Table (Node).Kind = N_Package_Declaration));

      if In_Tree.Project_Nodes.Table (Node).Kind = N_Project_Declaration then
         In_Tree.Project_Nodes.Table (Node).Field1 := To;
      else
         In_Tree.Project_Nodes.Table (Node).Field2 := To;
      end if;
   end Set_First_Declarative_Item_Of;

   ----------------------------------
   -- Set_First_Expression_In_List --
   ----------------------------------

   procedure Set_First_Expression_In_List
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id)
   is
   begin
      pragma Assert
        (Present (Node)
          and then
            In_Tree.Project_Nodes.Table (Node).Kind = N_Literal_String_List);
      In_Tree.Project_Nodes.Table (Node).Field1 := To;
   end Set_First_Expression_In_List;

   ------------------------------
   -- Set_First_Literal_String --
   ------------------------------

   procedure Set_First_Literal_String
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id)
   is
   begin
      pragma Assert
        (Present (Node)
          and then
         In_Tree.Project_Nodes.Table (Node).Kind =
           N_String_Type_Declaration);
      In_Tree.Project_Nodes.Table (Node).Field1 := To;
   end Set_First_Literal_String;

   --------------------------
   -- Set_First_Package_Of --
   --------------------------

   procedure Set_First_Package_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Package_Declaration_Id)
   is
   begin
      pragma Assert
        (Present (Node)
          and then
            In_Tree.Project_Nodes.Table (Node).Kind = N_Project);
      In_Tree.Project_Nodes.Table (Node).Packages := To;
   end Set_First_Package_Of;

   ------------------------------
   -- Set_First_String_Type_Of --
   ------------------------------

   procedure Set_First_String_Type_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id)
   is
   begin
      pragma Assert
        (Present (Node)
          and then
            In_Tree.Project_Nodes.Table (Node).Kind = N_Project);
      In_Tree.Project_Nodes.Table (Node).Field3 := To;
   end Set_First_String_Type_Of;

   --------------------
   -- Set_First_Term --
   --------------------

   procedure Set_First_Term
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id)
   is
   begin
      pragma Assert
        (Present (Node)
          and then
            In_Tree.Project_Nodes.Table (Node).Kind = N_Expression);
      In_Tree.Project_Nodes.Table (Node).Field1 := To;
   end Set_First_Term;

   ---------------------------
   -- Set_First_Variable_Of --
   ---------------------------

   procedure Set_First_Variable_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Variable_Node_Id)
   is
   begin
      pragma Assert
        (Present (Node)
          and then
            (In_Tree.Project_Nodes.Table (Node).Kind = N_Project
               or else
             In_Tree.Project_Nodes.Table (Node).Kind = N_Package_Declaration));
      In_Tree.Project_Nodes.Table (Node).Variables := To;
   end Set_First_Variable_Of;

   ------------------------------
   -- Set_First_With_Clause_Of --
   ------------------------------

   procedure Set_First_With_Clause_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id)
   is
   begin
      pragma Assert
        (Present (Node)
          and then
            In_Tree.Project_Nodes.Table (Node).Kind = N_Project);
      In_Tree.Project_Nodes.Table (Node).Field1 := To;
   end Set_First_With_Clause_Of;

   --------------------------
   -- Set_Is_Extending_All --
   --------------------------

   procedure Set_Is_Extending_All
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref)
   is
   begin
      pragma Assert
        (Present (Node)
          and then
            (In_Tree.Project_Nodes.Table (Node).Kind = N_Project
               or else
             In_Tree.Project_Nodes.Table (Node).Kind = N_With_Clause));
      In_Tree.Project_Nodes.Table (Node).Flag2 := True;
   end Set_Is_Extending_All;

   -----------------------------
   -- Set_Is_Not_Last_In_List --
   -----------------------------

   procedure Set_Is_Not_Last_In_List
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref)
   is
   begin
      pragma Assert
        (Present (Node)
          and then
             In_Tree.Project_Nodes.Table (Node).Kind = N_With_Clause);
      In_Tree.Project_Nodes.Table (Node).Flag1 := True;
   end Set_Is_Not_Last_In_List;

   -----------------
   -- Set_Kind_Of --
   -----------------

   procedure Set_Kind_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Kind)
   is
   begin
      pragma Assert (Present (Node));
      In_Tree.Project_Nodes.Table (Node).Kind := To;
   end Set_Kind_Of;

   ---------------------
   -- Set_Location_Of --
   ---------------------

   procedure Set_Location_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Source_Ptr)
   is
   begin
      pragma Assert (Present (Node));
      In_Tree.Project_Nodes.Table (Node).Location := To;
   end Set_Location_Of;

   -----------------------------
   -- Set_Extended_Project_Of --
   -----------------------------

   procedure Set_Extended_Project_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id)
   is
   begin
      pragma Assert
        (Present (Node)
          and then
            In_Tree.Project_Nodes.Table (Node).Kind = N_Project_Declaration);
      In_Tree.Project_Nodes.Table (Node).Field2 := To;
   end Set_Extended_Project_Of;

   ----------------------------------
   -- Set_Extended_Project_Path_Of --
   ----------------------------------

   procedure Set_Extended_Project_Path_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Path_Name_Type)
   is
   begin
      pragma Assert
        (Present (Node)
          and then
            In_Tree.Project_Nodes.Table (Node).Kind = N_Project);
      In_Tree.Project_Nodes.Table (Node).Value := Name_Id (To);
   end Set_Extended_Project_Path_Of;

   ------------------------------
   -- Set_Extending_Project_Of --
   ------------------------------

   procedure Set_Extending_Project_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id)
   is
   begin
      pragma Assert
        (Present (Node)
          and then
            In_Tree.Project_Nodes.Table (Node).Kind = N_Project_Declaration);
      In_Tree.Project_Nodes.Table (Node).Field3 := To;
   end Set_Extending_Project_Of;

   -----------------
   -- Set_Name_Of --
   -----------------

   procedure Set_Name_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Name_Id)
   is
   begin
      pragma Assert (Present (Node));
      In_Tree.Project_Nodes.Table (Node).Name := To;
   end Set_Name_Of;

   -------------------------------
   -- Set_Next_Declarative_Item --
   -------------------------------

   procedure Set_Next_Declarative_Item
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id)
   is
   begin
      pragma Assert
        (Present (Node)
          and then
            In_Tree.Project_Nodes.Table (Node).Kind = N_Declarative_Item);
      In_Tree.Project_Nodes.Table (Node).Field2 := To;
   end Set_Next_Declarative_Item;

   -----------------------
   -- Set_Next_End_Node --
   -----------------------

   procedure Set_Next_End_Node (To : Project_Node_Id) is
   begin
      Next_End_Nodes.Increment_Last;
      Next_End_Nodes.Table (Next_End_Nodes.Last) := To;
   end Set_Next_End_Node;

   ---------------------------------
   -- Set_Next_Expression_In_List --
   ---------------------------------

   procedure Set_Next_Expression_In_List
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id)
   is
   begin
      pragma Assert
        (Present (Node)
          and then
            In_Tree.Project_Nodes.Table (Node).Kind = N_Expression);
      In_Tree.Project_Nodes.Table (Node).Field2 := To;
   end Set_Next_Expression_In_List;

   -----------------------------
   -- Set_Next_Literal_String --
   -----------------------------

   procedure Set_Next_Literal_String
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id)
   is
   begin
      pragma Assert
        (Present (Node)
          and then
            In_Tree.Project_Nodes.Table (Node).Kind = N_Literal_String);
      In_Tree.Project_Nodes.Table (Node).Field1 := To;
   end Set_Next_Literal_String;

   ---------------------------------
   -- Set_Next_Package_In_Project --
   ---------------------------------

   procedure Set_Next_Package_In_Project
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id)
   is
   begin
      pragma Assert
        (Present (Node)
          and then
            In_Tree.Project_Nodes.Table (Node).Kind = N_Package_Declaration);
      In_Tree.Project_Nodes.Table (Node).Field3 := To;
   end Set_Next_Package_In_Project;

   --------------------------
   -- Set_Next_String_Type --
   --------------------------

   procedure Set_Next_String_Type
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id)
   is
   begin
      pragma Assert
        (Present (Node)
          and then
         In_Tree.Project_Nodes.Table (Node).Kind =
           N_String_Type_Declaration);
      In_Tree.Project_Nodes.Table (Node).Field2 := To;
   end Set_Next_String_Type;

   -------------------
   -- Set_Next_Term --
   -------------------

   procedure Set_Next_Term
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id)
   is
   begin
      pragma Assert
        (Present (Node)
          and then
            In_Tree.Project_Nodes.Table (Node).Kind = N_Term);
      In_Tree.Project_Nodes.Table (Node).Field2 := To;
   end Set_Next_Term;

   -----------------------
   -- Set_Next_Variable --
   -----------------------

   procedure Set_Next_Variable
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id)
   is
   begin
      pragma Assert
        (Present (Node)
          and then
           (In_Tree.Project_Nodes.Table (Node).Kind =
              N_Typed_Variable_Declaration
               or else
            In_Tree.Project_Nodes.Table (Node).Kind =
              N_Variable_Declaration));
      In_Tree.Project_Nodes.Table (Node).Field3 := To;
   end Set_Next_Variable;

   -----------------------------
   -- Set_Next_With_Clause_Of --
   -----------------------------

   procedure Set_Next_With_Clause_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id)
   is
   begin
      pragma Assert
        (Present (Node)
          and then
            In_Tree.Project_Nodes.Table (Node).Kind = N_With_Clause);
      In_Tree.Project_Nodes.Table (Node).Field2 := To;
   end Set_Next_With_Clause_Of;

   -----------------------
   -- Set_Package_Id_Of --
   -----------------------

   procedure Set_Package_Id_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Package_Node_Id)
   is
   begin
      pragma Assert
        (Present (Node)
          and then
            In_Tree.Project_Nodes.Table (Node).Kind = N_Package_Declaration);
      In_Tree.Project_Nodes.Table (Node).Pkg_Id := To;
   end Set_Package_Id_Of;

   -------------------------
   -- Set_Package_Node_Of --
   -------------------------

   procedure Set_Package_Node_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id)
   is
   begin
      pragma Assert
        (Present (Node)
          and then
            (In_Tree.Project_Nodes.Table (Node).Kind = N_Variable_Reference
               or else
             In_Tree.Project_Nodes.Table (Node).Kind = N_Attribute_Reference));
      In_Tree.Project_Nodes.Table (Node).Field2 := To;
   end Set_Package_Node_Of;

   ----------------------
   -- Set_Path_Name_Of --
   ----------------------

   procedure Set_Path_Name_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Path_Name_Type)
   is
   begin
      pragma Assert
        (Present (Node)
          and then
            (In_Tree.Project_Nodes.Table (Node).Kind = N_Project
               or else
             In_Tree.Project_Nodes.Table (Node).Kind = N_With_Clause));
      In_Tree.Project_Nodes.Table (Node).Path_Name := To;
   end Set_Path_Name_Of;

   ---------------------------
   -- Set_Previous_End_Node --
   ---------------------------
   procedure Set_Previous_End_Node (To : Project_Node_Id) is
   begin
      Previous_End_Node := To;
   end Set_Previous_End_Node;

   ----------------------------
   -- Set_Previous_Line_Node --
   ----------------------------

   procedure Set_Previous_Line_Node (To : Project_Node_Id) is
   begin
      Previous_Line_Node := To;
   end Set_Previous_Line_Node;

   --------------------------------
   -- Set_Project_Declaration_Of --
   --------------------------------

   procedure Set_Project_Declaration_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id)
   is
   begin
      pragma Assert
        (Present (Node)
         and then
           In_Tree.Project_Nodes.Table (Node).Kind = N_Project);
      In_Tree.Project_Nodes.Table (Node).Field2 := To;
   end Set_Project_Declaration_Of;

   ------------------------------
   -- Set_Project_Qualifier_Of --
   ------------------------------

   procedure Set_Project_Qualifier_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Qualifier)
   is
   begin
      pragma Assert
        (Present (Node)
          and then In_Tree.Project_Nodes.Table (Node).Kind = N_Project);
      In_Tree.Project_Nodes.Table (Node).Qualifier := To;
   end Set_Project_Qualifier_Of;

   ---------------------------
   -- Set_Parent_Project_Of --
   ---------------------------

   procedure Set_Parent_Project_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id)
   is
   begin
      pragma Assert
        (Present (Node)
          and then In_Tree.Project_Nodes.Table (Node).Kind = N_Project);
      In_Tree.Project_Nodes.Table (Node).Field4 := To;
   end Set_Parent_Project_Of;

   -----------------------------------------------
   -- Set_Project_File_Includes_Unkept_Comments --
   -----------------------------------------------

   procedure Set_Project_File_Includes_Unkept_Comments
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Boolean)
   is
      Declaration : constant Project_Node_Id :=
                      Project_Declaration_Of (Node, In_Tree);
   begin
      In_Tree.Project_Nodes.Table (Declaration).Flag1 := To;
   end Set_Project_File_Includes_Unkept_Comments;

   -------------------------
   -- Set_Project_Node_Of --
   -------------------------

   procedure Set_Project_Node_Of
     (Node         : Project_Node_Id;
      In_Tree      : Project_Node_Tree_Ref;
      To           : Project_Node_Id;
      Limited_With : Boolean := False)
   is
   begin
      pragma Assert
        (Present (Node)
          and then
            (In_Tree.Project_Nodes.Table (Node).Kind = N_With_Clause
               or else
             In_Tree.Project_Nodes.Table (Node).Kind = N_Variable_Reference
               or else
             In_Tree.Project_Nodes.Table (Node).Kind = N_Attribute_Reference));
      In_Tree.Project_Nodes.Table (Node).Field1 := To;

      if In_Tree.Project_Nodes.Table (Node).Kind = N_With_Clause
        and then not Limited_With
      then
         In_Tree.Project_Nodes.Table (Node).Field3 := To;
      end if;
   end Set_Project_Node_Of;

   ---------------------------------------
   -- Set_Project_Of_Renamed_Package_Of --
   ---------------------------------------

   procedure Set_Project_Of_Renamed_Package_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id)
   is
   begin
      pragma Assert
        (Present (Node)
          and then
            In_Tree.Project_Nodes.Table (Node).Kind = N_Package_Declaration);
      In_Tree.Project_Nodes.Table (Node).Field1 := To;
   end Set_Project_Of_Renamed_Package_Of;

   -------------------------
   -- Set_Source_Index_Of --
   -------------------------

   procedure Set_Source_Index_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Int)
   is
   begin
      pragma Assert
        (Present (Node)
          and then
           (In_Tree.Project_Nodes.Table (Node).Kind = N_Literal_String
            or else
            In_Tree.Project_Nodes.Table (Node).Kind =
              N_Attribute_Declaration));
      In_Tree.Project_Nodes.Table (Node).Src_Index := To;
   end Set_Source_Index_Of;

   ------------------------
   -- Set_String_Type_Of --
   ------------------------

   procedure Set_String_Type_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Project_Node_Id)
   is
   begin
      pragma Assert
        (Present (Node)
          and then
           (In_Tree.Project_Nodes.Table (Node).Kind =
              N_Variable_Reference
               or else
            In_Tree.Project_Nodes.Table (Node).Kind =
              N_Typed_Variable_Declaration)
          and then
            In_Tree.Project_Nodes.Table (To).Kind = N_String_Type_Declaration);

      if In_Tree.Project_Nodes.Table (Node).Kind = N_Variable_Reference then
         In_Tree.Project_Nodes.Table (Node).Field3 := To;
      else
         In_Tree.Project_Nodes.Table (Node).Field2 := To;
      end if;
   end Set_String_Type_Of;

   -------------------------
   -- Set_String_Value_Of --
   -------------------------

   procedure Set_String_Value_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref;
      To      : Name_Id)
   is
   begin
      pragma Assert
        (Present (Node)
          and then
            (In_Tree.Project_Nodes.Table (Node).Kind = N_With_Clause
               or else
             In_Tree.Project_Nodes.Table (Node).Kind = N_Comment
               or else
             In_Tree.Project_Nodes.Table (Node).Kind = N_Literal_String));
      In_Tree.Project_Nodes.Table (Node).Value := To;
   end Set_String_Value_Of;

   ---------------------
   -- Source_Index_Of --
   ---------------------

   function Source_Index_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Int
   is
   begin
      pragma Assert
        (Present (Node)
          and then
            (In_Tree.Project_Nodes.Table (Node).Kind = N_Literal_String
              or else
             In_Tree.Project_Nodes.Table (Node).Kind =
               N_Attribute_Declaration));
      return In_Tree.Project_Nodes.Table (Node).Src_Index;
   end Source_Index_Of;

   --------------------
   -- String_Type_Of --
   --------------------

   function String_Type_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Project_Node_Id
   is
   begin
      pragma Assert
        (Present (Node)
          and then
           (In_Tree.Project_Nodes.Table (Node).Kind =
              N_Variable_Reference
               or else
            In_Tree.Project_Nodes.Table (Node).Kind =
              N_Typed_Variable_Declaration));

      if In_Tree.Project_Nodes.Table (Node).Kind = N_Variable_Reference then
         return In_Tree.Project_Nodes.Table (Node).Field3;
      else
         return In_Tree.Project_Nodes.Table (Node).Field2;
      end if;
   end String_Type_Of;

   ---------------------
   -- String_Value_Of --
   ---------------------

   function String_Value_Of
     (Node    : Project_Node_Id;
      In_Tree : Project_Node_Tree_Ref) return Name_Id
   is
   begin
      pragma Assert
        (Present (Node)
          and then
           (In_Tree.Project_Nodes.Table (Node).Kind = N_With_Clause
              or else
            In_Tree.Project_Nodes.Table (Node).Kind = N_Comment
               or else
            In_Tree.Project_Nodes.Table (Node).Kind = N_Literal_String));
      return In_Tree.Project_Nodes.Table (Node).Value;
   end String_Value_Of;

   --------------------
   -- Value_Is_Valid --
   --------------------

   function Value_Is_Valid
     (For_Typed_Variable : Project_Node_Id;
      In_Tree            : Project_Node_Tree_Ref;
      Value              : Name_Id) return Boolean
   is
   begin
      pragma Assert
        (Present (For_Typed_Variable)
          and then
           (In_Tree.Project_Nodes.Table (For_Typed_Variable).Kind =
                                     N_Typed_Variable_Declaration));

      declare
         Current_String : Project_Node_Id :=
                            First_Literal_String
                              (String_Type_Of (For_Typed_Variable, In_Tree),
                               In_Tree);

      begin
         while Present (Current_String)
           and then
             String_Value_Of (Current_String, In_Tree) /= Value
         loop
            Current_String :=
              Next_Literal_String (Current_String, In_Tree);
         end loop;

         return Present (Current_String);
      end;

   end Value_Is_Valid;

   -------------------------------
   -- There_Are_Unkept_Comments --
   -------------------------------

   function There_Are_Unkept_Comments return Boolean is
   begin
      return Unkept_Comments;
   end There_Are_Unkept_Comments;

end Prj.Tree;
