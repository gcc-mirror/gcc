------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              P R J . T R E E                             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.2 $
--                                                                          --
--             Copyright (C) 2001 Free Software Foundation, Inc.            --
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

with Stringt; use Stringt;

package body Prj.Tree is

   use Tree_Private_Part;

   --------------------------------
   -- Associative_Array_Index_Of --
   --------------------------------

   function Associative_Array_Index_Of
     (Node : Project_Node_Id)
      return String_Id
   is
   begin
      pragma Assert
        (Node /= Empty_Node
          and then
            (Project_Nodes.Table (Node).Kind = N_Attribute_Declaration));
      return Project_Nodes.Table (Node).Value;
   end Associative_Array_Index_Of;

   ----------------------
   -- Case_Insensitive --
   ----------------------

   function Case_Insensitive (Node : Project_Node_Id) return Boolean is
   begin
      pragma Assert
        (Node /= Empty_Node
          and then
            (Project_Nodes.Table (Node).Kind = N_Attribute_Declaration));
      return Project_Nodes.Table (Node).Case_Insensitive;
   end Case_Insensitive;

   --------------------------------
   -- Case_Variable_Reference_Of --
   --------------------------------

   function Case_Variable_Reference_Of
     (Node : Project_Node_Id)
      return Project_Node_Id
   is
   begin
      pragma Assert
        (Node /= Empty_Node
          and then
            Project_Nodes.Table (Node).Kind = N_Case_Construction);
      return Project_Nodes.Table (Node).Field1;
   end Case_Variable_Reference_Of;

   -----------------------
   -- Current_Item_Node --
   -----------------------

   function Current_Item_Node
     (Node : Project_Node_Id)
      return Project_Node_Id
   is
   begin
      pragma Assert
        (Node /= Empty_Node
          and then
            Project_Nodes.Table (Node).Kind = N_Declarative_Item);
      return Project_Nodes.Table (Node).Field1;
   end Current_Item_Node;

   ------------------
   -- Current_Term --
   ------------------

   function Current_Term
     (Node : Project_Node_Id)
      return Project_Node_Id
   is
   begin
      pragma Assert
        (Node /= Empty_Node
          and then
            Project_Nodes.Table (Node).Kind = N_Term);
      return Project_Nodes.Table (Node).Field1;
   end Current_Term;

   --------------------------
   -- Default_Project_Node --
   --------------------------

   function Default_Project_Node
     (Of_Kind       : Project_Node_Kind;
      And_Expr_Kind : Variable_Kind := Undefined)
      return          Project_Node_Id
   is
   begin
      Project_Nodes.Increment_Last;
      Project_Nodes.Table (Project_Nodes.Last) :=
           (Kind             => Of_Kind,
            Location         => No_Location,
            Directory        => No_Name,
            Expr_Kind        => And_Expr_Kind,
            Variables        => Empty_Node,
            Packages         => Empty_Node,
            Pkg_Id           => Empty_Package,
            Name             => No_Name,
            Path_Name        => No_Name,
            Value            => No_String,
            Field1           => Empty_Node,
            Field2           => Empty_Node,
            Field3           => Empty_Node,
            Case_Insensitive => False);
      return Project_Nodes.Last;
   end Default_Project_Node;

   ------------------
   -- Directory_Of --
   ------------------

   function Directory_Of (Node : Project_Node_Id) return Name_Id is
   begin
      pragma Assert
        (Node /= Empty_Node
          and then
            Project_Nodes.Table (Node).Kind = N_Project);
      return Project_Nodes.Table (Node).Directory;
   end Directory_Of;

   ------------------------
   -- Expression_Kind_Of --
   ------------------------

   function Expression_Kind_Of (Node : Project_Node_Id) return Variable_Kind is
   begin
      pragma Assert
        (Node /= Empty_Node
           and then
             (Project_Nodes.Table (Node).Kind = N_Literal_String
                or else
              Project_Nodes.Table (Node).Kind = N_Attribute_Declaration
                or else
              Project_Nodes.Table (Node).Kind = N_Variable_Declaration
                or else
              Project_Nodes.Table (Node).Kind = N_Typed_Variable_Declaration
                or else
              Project_Nodes.Table (Node).Kind = N_Expression
                or else
              Project_Nodes.Table (Node).Kind = N_Term
                or else
              Project_Nodes.Table (Node).Kind = N_Variable_Reference
                or else
              Project_Nodes.Table (Node).Kind = N_Attribute_Reference));

      return Project_Nodes.Table (Node).Expr_Kind;
   end Expression_Kind_Of;

   -------------------
   -- Expression_Of --
   -------------------

   function Expression_Of
     (Node : Project_Node_Id)
      return Project_Node_Id
   is
   begin
      pragma Assert
        (Node /= Empty_Node
          and then
            (Project_Nodes.Table (Node).Kind = N_Attribute_Declaration
               or else
             Project_Nodes.Table (Node).Kind = N_Typed_Variable_Declaration
               or else
             Project_Nodes.Table (Node).Kind = N_Variable_Declaration));

      return Project_Nodes.Table (Node).Field1;
   end Expression_Of;

   ---------------------------
   -- External_Reference_Of --
   ---------------------------

   function External_Reference_Of
     (Node : Project_Node_Id)
      return Project_Node_Id
   is
   begin
      pragma Assert
        (Node /= Empty_Node
          and then
            Project_Nodes.Table (Node).Kind = N_External_Value);
      return Project_Nodes.Table (Node).Field1;
   end External_Reference_Of;

   -------------------------
   -- External_Default_Of --
   -------------------------

   function External_Default_Of
     (Node : Project_Node_Id)
      return Project_Node_Id
   is
   begin
      pragma Assert
        (Node /= Empty_Node
          and then
            Project_Nodes.Table (Node).Kind = N_External_Value);
      return Project_Nodes.Table (Node).Field2;
   end External_Default_Of;

   ------------------------
   -- First_Case_Item_Of --
   ------------------------

   function First_Case_Item_Of
     (Node : Project_Node_Id)
      return Project_Node_Id
   is
   begin
      pragma Assert
        (Node /= Empty_Node
          and then
            Project_Nodes.Table (Node).Kind = N_Case_Construction);
      return Project_Nodes.Table (Node).Field2;
   end First_Case_Item_Of;

   ---------------------
   -- First_Choice_Of --
   ---------------------

   function First_Choice_Of
     (Node : Project_Node_Id)
      return Project_Node_Id
   is
   begin
      pragma Assert
        (Node /= Empty_Node
          and then
            Project_Nodes.Table (Node).Kind = N_Case_Item);
      return Project_Nodes.Table (Node).Field1;
   end First_Choice_Of;

   -------------------------------
   -- First_Declarative_Item_Of --
   -------------------------------

   function First_Declarative_Item_Of
     (Node : Project_Node_Id)
      return Project_Node_Id
   is
   begin
      pragma Assert
        (Node /= Empty_Node
          and then
            (Project_Nodes.Table (Node).Kind = N_Project_Declaration
               or else
             Project_Nodes.Table (Node).Kind = N_Case_Item
               or else
             Project_Nodes.Table (Node).Kind = N_Package_Declaration));

      if Project_Nodes.Table (Node).Kind = N_Project_Declaration then
         return Project_Nodes.Table (Node).Field1;
      else
         return Project_Nodes.Table (Node).Field2;
      end if;
   end First_Declarative_Item_Of;

   ------------------------------
   -- First_Expression_In_List --
   ------------------------------

   function First_Expression_In_List
     (Node : Project_Node_Id)
      return Project_Node_Id
   is
   begin
      pragma Assert
        (Node /= Empty_Node
          and then
            Project_Nodes.Table (Node).Kind = N_Literal_String_List);
      return Project_Nodes.Table (Node).Field1;
   end First_Expression_In_List;

   --------------------------
   -- First_Literal_String --
   --------------------------

   function First_Literal_String
     (Node : Project_Node_Id)
      return Project_Node_Id
   is
   begin
      pragma Assert
        (Node /= Empty_Node
          and then
            Project_Nodes.Table (Node).Kind = N_String_Type_Declaration);
      return Project_Nodes.Table (Node).Field1;
   end First_Literal_String;

   ----------------------
   -- First_Package_Of --
   ----------------------

   function First_Package_Of
     (Node : Project_Node_Id)
      return Package_Declaration_Id
   is
   begin
      pragma Assert
        (Node /= Empty_Node
          and then
            Project_Nodes.Table (Node).Kind = N_Project);
      return Project_Nodes.Table (Node).Packages;
   end First_Package_Of;

   --------------------------
   -- First_String_Type_Of --
   --------------------------

   function First_String_Type_Of
     (Node : Project_Node_Id)
      return Project_Node_Id
   is
   begin
      pragma Assert
        (Node /= Empty_Node
          and then
            Project_Nodes.Table (Node).Kind = N_Project);
      return Project_Nodes.Table (Node).Field3;
   end First_String_Type_Of;

   ----------------
   -- First_Term --
   ----------------

   function First_Term
     (Node : Project_Node_Id)
      return Project_Node_Id
   is
   begin
      pragma Assert
        (Node /= Empty_Node
          and then
            Project_Nodes.Table (Node).Kind = N_Expression);
      return Project_Nodes.Table (Node).Field1;
   end First_Term;

   -----------------------
   -- First_Variable_Of --
   -----------------------

   function First_Variable_Of
     (Node : Project_Node_Id)
      return Variable_Node_Id
   is
   begin
      pragma Assert
        (Node /= Empty_Node
          and then
            (Project_Nodes.Table (Node).Kind = N_Project
               or else
             Project_Nodes.Table (Node).Kind = N_Package_Declaration));

      return Project_Nodes.Table (Node).Variables;
   end First_Variable_Of;

   --------------------------
   -- First_With_Clause_Of --
   --------------------------

   function First_With_Clause_Of
     (Node : Project_Node_Id)
      return Project_Node_Id
   is
   begin
      pragma Assert
        (Node /= Empty_Node
          and then
            Project_Nodes.Table (Node).Kind = N_Project);
      return Project_Nodes.Table (Node).Field1;
   end First_With_Clause_Of;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Project_Nodes.Set_Last (Empty_Node);
      Projects_Htable.Reset;
   end Initialize;

   -------------
   -- Kind_Of --
   -------------

   function Kind_Of (Node : Project_Node_Id) return Project_Node_Kind is
   begin
      pragma Assert (Node /= Empty_Node);
      return Project_Nodes.Table (Node).Kind;
   end Kind_Of;

   -----------------
   -- Location_Of --
   -----------------

   function Location_Of (Node : Project_Node_Id) return Source_Ptr is
   begin
      pragma Assert (Node /= Empty_Node);
      return Project_Nodes.Table (Node).Location;
   end Location_Of;

   -------------------------
   -- Modified_Project_Of --
   -------------------------

   function Modified_Project_Of
     (Node : Project_Node_Id)
      return Project_Node_Id
   is
   begin
      pragma Assert
        (Node /= Empty_Node
          and then
            Project_Nodes.Table (Node).Kind = N_Project_Declaration);
      return Project_Nodes.Table (Node).Field2;
   end Modified_Project_Of;

   ------------------------------
   -- Modified_Project_Path_Of --
   ------------------------------

   function Modified_Project_Path_Of
     (Node : Project_Node_Id)
      return String_Id
   is
   begin
      pragma Assert
        (Node /= Empty_Node
          and then
            Project_Nodes.Table (Node).Kind = N_Project);
      return Project_Nodes.Table (Node).Value;
   end Modified_Project_Path_Of;

   -------------
   -- Name_Of --
   -------------

   function Name_Of (Node : Project_Node_Id) return Name_Id is
   begin
      pragma Assert (Node /= Empty_Node);
      return Project_Nodes.Table (Node).Name;
   end Name_Of;

   --------------------
   -- Next_Case_Item --
   --------------------

   function Next_Case_Item
     (Node : Project_Node_Id)
      return Project_Node_Id
   is
   begin
      pragma Assert
        (Node /= Empty_Node
          and then
            Project_Nodes.Table (Node).Kind = N_Case_Item);
      return Project_Nodes.Table (Node).Field3;
   end Next_Case_Item;

   ---------------------------
   -- Next_Declarative_Item --
   ---------------------------

   function Next_Declarative_Item
     (Node : Project_Node_Id)
      return Project_Node_Id
   is
   begin
      pragma Assert
        (Node /= Empty_Node
          and then
            Project_Nodes.Table (Node).Kind = N_Declarative_Item);
      return Project_Nodes.Table (Node).Field2;
   end Next_Declarative_Item;

   -----------------------------
   -- Next_Expression_In_List --
   -----------------------------

   function Next_Expression_In_List
     (Node : Project_Node_Id)
      return Project_Node_Id
   is
   begin
      pragma Assert
        (Node /= Empty_Node
          and then
            Project_Nodes.Table (Node).Kind = N_Expression);
      return Project_Nodes.Table (Node).Field2;
   end Next_Expression_In_List;

   -------------------------
   -- Next_Literal_String --
   -------------------------

   function Next_Literal_String
     (Node : Project_Node_Id)
      return Project_Node_Id
   is
   begin
      pragma Assert
        (Node /= Empty_Node
          and then
            Project_Nodes.Table (Node).Kind = N_Literal_String);
      return Project_Nodes.Table (Node).Field1;
   end Next_Literal_String;

   -----------------------------
   -- Next_Package_In_Project --
   -----------------------------

   function Next_Package_In_Project
     (Node : Project_Node_Id)
      return Project_Node_Id
   is
   begin
      pragma Assert
        (Node /= Empty_Node
          and then
            Project_Nodes.Table (Node).Kind = N_Package_Declaration);
      return Project_Nodes.Table (Node).Field3;
   end Next_Package_In_Project;

   ----------------------
   -- Next_String_Type --
   ----------------------

   function Next_String_Type
     (Node : Project_Node_Id)
      return Project_Node_Id
   is
   begin
      pragma Assert
        (Node /= Empty_Node
          and then
            Project_Nodes.Table (Node).Kind = N_String_Type_Declaration);
      return Project_Nodes.Table (Node).Field2;
   end Next_String_Type;

   ---------------
   -- Next_Term --
   ---------------

   function Next_Term
     (Node : Project_Node_Id)
      return Project_Node_Id
   is
   begin
      pragma Assert
        (Node /= Empty_Node
          and then
            Project_Nodes.Table (Node).Kind = N_Term);
      return Project_Nodes.Table (Node).Field2;
   end Next_Term;

   -------------------
   -- Next_Variable --
   -------------------

   function Next_Variable
     (Node : Project_Node_Id)
      return Project_Node_Id
   is
   begin
      pragma Assert
        (Node /= Empty_Node
          and then
            (Project_Nodes.Table (Node).Kind = N_Typed_Variable_Declaration
               or else
             Project_Nodes.Table (Node).Kind = N_Variable_Declaration));

      return Project_Nodes.Table (Node).Field3;
   end Next_Variable;

   -------------------------
   -- Next_With_Clause_Of --
   -------------------------

   function Next_With_Clause_Of
     (Node : Project_Node_Id)
      return Project_Node_Id
   is
   begin
      pragma Assert
        (Node /= Empty_Node
          and then
            Project_Nodes.Table (Node).Kind = N_With_Clause);
      return Project_Nodes.Table (Node).Field2;
   end Next_With_Clause_Of;

   -------------------
   -- Package_Id_Of --
   -------------------

   function Package_Id_Of (Node : Project_Node_Id) return Package_Node_Id is
   begin
      pragma Assert
        (Node /= Empty_Node
          and then
            Project_Nodes.Table (Node).Kind = N_Package_Declaration);
      return Project_Nodes.Table (Node).Pkg_Id;
   end Package_Id_Of;

   ---------------------
   -- Package_Node_Of --
   ---------------------

   function Package_Node_Of
     (Node : Project_Node_Id)
      return Project_Node_Id
   is
   begin
      pragma Assert
        (Node /= Empty_Node
          and then
            (Project_Nodes.Table (Node).Kind = N_Variable_Reference
               or else
             Project_Nodes.Table (Node).Kind = N_Attribute_Reference));
      return Project_Nodes.Table (Node).Field2;
   end Package_Node_Of;

   ------------------
   -- Path_Name_Of --
   ------------------

   function Path_Name_Of (Node : Project_Node_Id) return Name_Id is
   begin
      pragma Assert
        (Node /= Empty_Node
          and then
            (Project_Nodes.Table (Node).Kind = N_Project
               or else
             Project_Nodes.Table (Node).Kind = N_With_Clause));
      return Project_Nodes.Table (Node).Path_Name;
   end Path_Name_Of;

   ----------------------------
   -- Project_Declaration_Of --
   ----------------------------

   function Project_Declaration_Of
     (Node : Project_Node_Id)
      return Project_Node_Id
   is
   begin
      pragma Assert
        (Node /= Empty_Node
          and then
            Project_Nodes.Table (Node).Kind = N_Project);
      return Project_Nodes.Table (Node).Field2;
   end Project_Declaration_Of;

   ---------------------
   -- Project_Node_Of --
   ---------------------

   function Project_Node_Of
     (Node : Project_Node_Id)
      return Project_Node_Id
   is
   begin
      pragma Assert
        (Node /= Empty_Node
          and then
           (Project_Nodes.Table (Node).Kind = N_With_Clause
              or else
            Project_Nodes.Table (Node).Kind = N_Variable_Reference
              or else
            Project_Nodes.Table (Node).Kind = N_Attribute_Reference));
      return Project_Nodes.Table (Node).Field1;
   end Project_Node_Of;

   -----------------------------------
   -- Project_Of_Renamed_Package_Of --
   -----------------------------------

   function Project_Of_Renamed_Package_Of
     (Node : Project_Node_Id)
      return Project_Node_Id
   is
   begin
      pragma Assert
        (Node /= Empty_Node
          and then
            Project_Nodes.Table (Node).Kind = N_Package_Declaration);
      return Project_Nodes.Table (Node).Field1;
   end Project_Of_Renamed_Package_Of;

   ------------------------------------
   -- Set_Associative_Array_Index_Of --
   ------------------------------------

   procedure Set_Associative_Array_Index_Of
     (Node : Project_Node_Id;
      To   : String_Id)
   is
   begin
      pragma Assert
        (Node /= Empty_Node
          and then
            Project_Nodes.Table (Node).Kind = N_Attribute_Declaration);
      Project_Nodes.Table (Node).Value := To;
   end Set_Associative_Array_Index_Of;

   --------------------------
   -- Set_Case_Insensitive --
   --------------------------

   procedure Set_Case_Insensitive
     (Node : Project_Node_Id;
      To   : Boolean)
   is
   begin
      pragma Assert
        (Node /= Empty_Node
          and then
            Project_Nodes.Table (Node).Kind = N_Attribute_Declaration);
      Project_Nodes.Table (Node).Case_Insensitive := To;
   end Set_Case_Insensitive;

   ------------------------------------
   -- Set_Case_Variable_Reference_Of --
   ------------------------------------

   procedure Set_Case_Variable_Reference_Of
     (Node : Project_Node_Id;
      To   : Project_Node_Id)
   is
   begin
      pragma Assert
        (Node /= Empty_Node
          and then
            Project_Nodes.Table (Node).Kind = N_Case_Construction);
      Project_Nodes.Table (Node).Field1 := To;
   end Set_Case_Variable_Reference_Of;

   ---------------------------
   -- Set_Current_Item_Node --
   ---------------------------

   procedure Set_Current_Item_Node
     (Node : Project_Node_Id;
      To   : Project_Node_Id)
   is
   begin
      pragma Assert
        (Node /= Empty_Node
          and then
            Project_Nodes.Table (Node).Kind = N_Declarative_Item);
      Project_Nodes.Table (Node).Field1 := To;
   end Set_Current_Item_Node;

   ----------------------
   -- Set_Current_Term --
   ----------------------

   procedure Set_Current_Term
     (Node : Project_Node_Id;
      To   : Project_Node_Id)
   is
   begin
      pragma Assert
        (Node /= Empty_Node
          and then
            Project_Nodes.Table (Node).Kind = N_Term);
      Project_Nodes.Table (Node).Field1 := To;
   end Set_Current_Term;

   ----------------------
   -- Set_Directory_Of --
   ----------------------

   procedure Set_Directory_Of
     (Node : Project_Node_Id;
      To   : Name_Id)
   is
   begin
      pragma Assert
        (Node /= Empty_Node
          and then
            Project_Nodes.Table (Node).Kind = N_Project);
      Project_Nodes.Table (Node).Directory := To;
   end Set_Directory_Of;

   ----------------------------
   -- Set_Expression_Kind_Of --
   ----------------------------

   procedure Set_Expression_Kind_Of
     (Node : Project_Node_Id;
      To   : Variable_Kind)
   is
   begin
      pragma Assert
        (Node /= Empty_Node
           and then
             (Project_Nodes.Table (Node).Kind = N_Literal_String
                or else
              Project_Nodes.Table (Node).Kind = N_Attribute_Declaration
                or else
              Project_Nodes.Table (Node).Kind = N_Variable_Declaration
                or else
              Project_Nodes.Table (Node).Kind = N_Typed_Variable_Declaration
                or else
              Project_Nodes.Table (Node).Kind = N_Expression
                or else
              Project_Nodes.Table (Node).Kind = N_Term
                or else
              Project_Nodes.Table (Node).Kind = N_Variable_Reference
                or else
              Project_Nodes.Table (Node).Kind = N_Attribute_Reference));
      Project_Nodes.Table (Node).Expr_Kind := To;
   end Set_Expression_Kind_Of;

   -----------------------
   -- Set_Expression_Of --
   -----------------------

   procedure Set_Expression_Of
     (Node : Project_Node_Id;
      To   : Project_Node_Id)
   is
   begin
      pragma Assert
        (Node /= Empty_Node
          and then
            (Project_Nodes.Table (Node).Kind = N_Attribute_Declaration
               or else
             Project_Nodes.Table (Node).Kind = N_Typed_Variable_Declaration
               or else
             Project_Nodes.Table (Node).Kind = N_Variable_Declaration));
      Project_Nodes.Table (Node).Field1 := To;
   end Set_Expression_Of;

   -------------------------------
   -- Set_External_Reference_Of --
   -------------------------------

   procedure Set_External_Reference_Of
     (Node : Project_Node_Id;
      To   : Project_Node_Id)
   is
   begin
      pragma Assert
        (Node /= Empty_Node
          and then
            Project_Nodes.Table (Node).Kind = N_External_Value);
      Project_Nodes.Table (Node).Field1 := To;
   end Set_External_Reference_Of;

   -----------------------------
   -- Set_External_Default_Of --
   -----------------------------

   procedure Set_External_Default_Of
     (Node : Project_Node_Id;
      To   : Project_Node_Id)
   is
   begin
      pragma Assert
        (Node /= Empty_Node
          and then
            Project_Nodes.Table (Node).Kind = N_External_Value);
      Project_Nodes.Table (Node).Field2 := To;
   end Set_External_Default_Of;

   ----------------------------
   -- Set_First_Case_Item_Of --
   ----------------------------

   procedure Set_First_Case_Item_Of
     (Node : Project_Node_Id;
      To   : Project_Node_Id)
   is
   begin
      pragma Assert
        (Node /= Empty_Node
          and then
            Project_Nodes.Table (Node).Kind = N_Case_Construction);
      Project_Nodes.Table (Node).Field2 := To;
   end Set_First_Case_Item_Of;

   -------------------------
   -- Set_First_Choice_Of --
   -------------------------

   procedure Set_First_Choice_Of
     (Node : Project_Node_Id;
      To   : Project_Node_Id)
   is
   begin
      pragma Assert
        (Node /= Empty_Node
          and then
            Project_Nodes.Table (Node).Kind = N_Case_Item);
      Project_Nodes.Table (Node).Field1 := To;
   end Set_First_Choice_Of;

   ------------------------
   -- Set_Next_Case_Item --
   ------------------------

   procedure Set_Next_Case_Item
     (Node : Project_Node_Id;
      To   : Project_Node_Id)
   is
   begin
      pragma Assert
        (Node /= Empty_Node
          and then
            Project_Nodes.Table (Node).Kind = N_Case_Item);
      Project_Nodes.Table (Node).Field3 := To;
   end Set_Next_Case_Item;

   -----------------------------------
   -- Set_First_Declarative_Item_Of --
   -----------------------------------

   procedure Set_First_Declarative_Item_Of
     (Node : Project_Node_Id;
      To   : Project_Node_Id)
   is
   begin
      pragma Assert
        (Node /= Empty_Node
          and then
            (Project_Nodes.Table (Node).Kind = N_Project_Declaration
               or else
             Project_Nodes.Table (Node).Kind = N_Case_Item
               or else
             Project_Nodes.Table (Node).Kind = N_Package_Declaration));

      if Project_Nodes.Table (Node).Kind = N_Project_Declaration then
         Project_Nodes.Table (Node).Field1 := To;
      else
         Project_Nodes.Table (Node).Field2 := To;
      end if;
   end Set_First_Declarative_Item_Of;

   ----------------------------------
   -- Set_First_Expression_In_List --
   ----------------------------------

   procedure Set_First_Expression_In_List
     (Node : Project_Node_Id;
      To   : Project_Node_Id)
   is
   begin
      pragma Assert
        (Node /= Empty_Node
          and then
            Project_Nodes.Table (Node).Kind = N_Literal_String_List);
      Project_Nodes.Table (Node).Field1 := To;
   end Set_First_Expression_In_List;

   ------------------------------
   -- Set_First_Literal_String --
   ------------------------------

   procedure Set_First_Literal_String
     (Node : Project_Node_Id;
      To   : Project_Node_Id)
   is
   begin
      pragma Assert
        (Node /= Empty_Node
          and then
            Project_Nodes.Table (Node).Kind = N_String_Type_Declaration);
      Project_Nodes.Table (Node).Field1 := To;
   end Set_First_Literal_String;

   --------------------------
   -- Set_First_Package_Of --
   --------------------------

   procedure Set_First_Package_Of
     (Node : Project_Node_Id;
      To   : Package_Declaration_Id)
   is
   begin
      pragma Assert
        (Node /= Empty_Node
          and then
            Project_Nodes.Table (Node).Kind = N_Project);
      Project_Nodes.Table (Node).Packages := To;
   end Set_First_Package_Of;

   ------------------------------
   -- Set_First_String_Type_Of --
   ------------------------------

   procedure Set_First_String_Type_Of
     (Node : Project_Node_Id;
      To   : Project_Node_Id)
   is
   begin
      pragma Assert
        (Node /= Empty_Node
          and then
            Project_Nodes.Table (Node).Kind = N_Project);
      Project_Nodes.Table (Node).Field3 := To;
   end Set_First_String_Type_Of;

   --------------------
   -- Set_First_Term --
   --------------------

   procedure Set_First_Term
     (Node : Project_Node_Id;
      To   : Project_Node_Id)
   is
   begin
      pragma Assert
        (Node /= Empty_Node
          and then
            Project_Nodes.Table (Node).Kind = N_Expression);
      Project_Nodes.Table (Node).Field1 := To;
   end Set_First_Term;

   ---------------------------
   -- Set_First_Variable_Of --
   ---------------------------

   procedure Set_First_Variable_Of
     (Node : Project_Node_Id;
      To   : Variable_Node_Id)
   is
   begin
      pragma Assert
        (Node /= Empty_Node
          and then
            (Project_Nodes.Table (Node).Kind = N_Project
               or else
             Project_Nodes.Table (Node).Kind = N_Package_Declaration));
      Project_Nodes.Table (Node).Variables := To;
   end Set_First_Variable_Of;

   ------------------------------
   -- Set_First_With_Clause_Of --
   ------------------------------

   procedure Set_First_With_Clause_Of
     (Node : Project_Node_Id;
      To   : Project_Node_Id)
   is
   begin
      pragma Assert
        (Node /= Empty_Node
          and then
            Project_Nodes.Table (Node).Kind = N_Project);
      Project_Nodes.Table (Node).Field1 := To;
   end Set_First_With_Clause_Of;

   -----------------
   -- Set_Kind_Of --
   -----------------

   procedure Set_Kind_Of
     (Node : Project_Node_Id;
      To   : Project_Node_Kind)
   is
   begin
      pragma Assert (Node /= Empty_Node);
      Project_Nodes.Table (Node).Kind := To;
   end Set_Kind_Of;

   ---------------------
   -- Set_Location_Of --
   ---------------------

   procedure Set_Location_Of
     (Node : Project_Node_Id;
      To   : Source_Ptr)
   is
   begin
      pragma Assert (Node /= Empty_Node);
      Project_Nodes.Table (Node).Location := To;
   end Set_Location_Of;

   -----------------------------
   -- Set_Modified_Project_Of --
   -----------------------------

   procedure Set_Modified_Project_Of
     (Node : Project_Node_Id;
      To   : Project_Node_Id)
   is
   begin
      pragma Assert
        (Node /= Empty_Node
          and then
            Project_Nodes.Table (Node).Kind = N_Project_Declaration);
      Project_Nodes.Table (Node).Field2 := To;
   end Set_Modified_Project_Of;

   ----------------------------------
   -- Set_Modified_Project_Path_Of --
   ----------------------------------

   procedure Set_Modified_Project_Path_Of
     (Node : Project_Node_Id;
      To   : String_Id)
   is
   begin
      pragma Assert
        (Node /= Empty_Node
          and then
            Project_Nodes.Table (Node).Kind = N_Project);
      Project_Nodes.Table (Node).Value := To;
   end Set_Modified_Project_Path_Of;

   -----------------
   -- Set_Name_Of --
   -----------------

   procedure Set_Name_Of
     (Node : Project_Node_Id;
      To   : Name_Id)
   is
   begin
      pragma Assert (Node /= Empty_Node);
      Project_Nodes.Table (Node).Name := To;
   end Set_Name_Of;

   -------------------------------
   -- Set_Next_Declarative_Item --
   -------------------------------

   procedure Set_Next_Declarative_Item
     (Node : Project_Node_Id;
      To   : Project_Node_Id)
   is
   begin
      pragma Assert
        (Node /= Empty_Node
          and then
            Project_Nodes.Table (Node).Kind = N_Declarative_Item);
      Project_Nodes.Table (Node).Field2 := To;
   end Set_Next_Declarative_Item;

   ---------------------------------
   -- Set_Next_Expression_In_List --
   ---------------------------------

   procedure Set_Next_Expression_In_List
     (Node : Project_Node_Id;
      To   : Project_Node_Id)
   is
   begin
      pragma Assert
        (Node /= Empty_Node
          and then
            Project_Nodes.Table (Node).Kind = N_Expression);
      Project_Nodes.Table (Node).Field2 := To;
   end Set_Next_Expression_In_List;

   -----------------------------
   -- Set_Next_Literal_String --
   -----------------------------

   procedure Set_Next_Literal_String
     (Node : Project_Node_Id;
      To   : Project_Node_Id)
   is
   begin
      pragma Assert
        (Node /= Empty_Node
          and then
            Project_Nodes.Table (Node).Kind = N_Literal_String);
      Project_Nodes.Table (Node).Field1 := To;
   end Set_Next_Literal_String;

   ---------------------------------
   -- Set_Next_Package_In_Project --
   ---------------------------------

   procedure Set_Next_Package_In_Project
     (Node : Project_Node_Id;
      To   : Project_Node_Id)
   is
   begin
      pragma Assert
        (Node /= Empty_Node
          and then
            Project_Nodes.Table (Node).Kind = N_Package_Declaration);
      Project_Nodes.Table (Node).Field3 := To;
   end Set_Next_Package_In_Project;

   --------------------------
   -- Set_Next_String_Type --
   --------------------------

   procedure Set_Next_String_Type
     (Node : Project_Node_Id;
      To   : Project_Node_Id)
   is
   begin
      pragma Assert
        (Node /= Empty_Node
          and then
            Project_Nodes.Table (Node).Kind = N_String_Type_Declaration);
      Project_Nodes.Table (Node).Field2 := To;
   end Set_Next_String_Type;

   -------------------
   -- Set_Next_Term --
   -------------------

   procedure Set_Next_Term
     (Node : Project_Node_Id;
      To   : Project_Node_Id)
   is
   begin
      pragma Assert
        (Node /= Empty_Node
          and then
            Project_Nodes.Table (Node).Kind = N_Term);
      Project_Nodes.Table (Node).Field2 := To;
   end Set_Next_Term;

   -----------------------
   -- Set_Next_Variable --
   -----------------------

   procedure Set_Next_Variable
     (Node : Project_Node_Id;
      To   : Project_Node_Id)
   is
   begin
      pragma Assert
        (Node /= Empty_Node
          and then
            (Project_Nodes.Table (Node).Kind = N_Typed_Variable_Declaration
               or else
             Project_Nodes.Table (Node).Kind = N_Variable_Declaration));
      Project_Nodes.Table (Node).Field3 := To;
   end Set_Next_Variable;

   -----------------------------
   -- Set_Next_With_Clause_Of --
   -----------------------------

   procedure Set_Next_With_Clause_Of
     (Node : Project_Node_Id;
      To   : Project_Node_Id)
   is
   begin
      pragma Assert
        (Node /= Empty_Node
          and then
            Project_Nodes.Table (Node).Kind = N_With_Clause);
      Project_Nodes.Table (Node).Field2 := To;
   end Set_Next_With_Clause_Of;

   -----------------------
   -- Set_Package_Id_Of --
   -----------------------

   procedure Set_Package_Id_Of
     (Node : Project_Node_Id;
      To   : Package_Node_Id)
   is
   begin
      pragma Assert
        (Node /= Empty_Node
          and then
            Project_Nodes.Table (Node).Kind = N_Package_Declaration);
      Project_Nodes.Table (Node).Pkg_Id := To;
   end Set_Package_Id_Of;

   -------------------------
   -- Set_Package_Node_Of --
   -------------------------

   procedure Set_Package_Node_Of
     (Node : Project_Node_Id;
      To   : Project_Node_Id)
   is
   begin
      pragma Assert
        (Node /= Empty_Node
          and then
            (Project_Nodes.Table (Node).Kind = N_Variable_Reference
               or else
             Project_Nodes.Table (Node).Kind = N_Attribute_Reference));
      Project_Nodes.Table (Node).Field2 := To;
   end Set_Package_Node_Of;

   ----------------------
   -- Set_Path_Name_Of --
   ----------------------

   procedure Set_Path_Name_Of
     (Node : Project_Node_Id;
      To   : Name_Id)
   is
   begin
      pragma Assert
        (Node /= Empty_Node
          and then
            (Project_Nodes.Table (Node).Kind = N_Project
               or else
             Project_Nodes.Table (Node).Kind = N_With_Clause));
      Project_Nodes.Table (Node).Path_Name := To;
   end Set_Path_Name_Of;

   --------------------------------
   -- Set_Project_Declaration_Of --
   --------------------------------

   procedure Set_Project_Declaration_Of
     (Node : Project_Node_Id;
      To   : Project_Node_Id)
   is
   begin
      pragma Assert
        (Node /= Empty_Node
         and then
           Project_Nodes.Table (Node).Kind = N_Project);
      Project_Nodes.Table (Node).Field2 := To;
   end Set_Project_Declaration_Of;

   -------------------------
   -- Set_Project_Node_Of --
   -------------------------

   procedure Set_Project_Node_Of
     (Node : Project_Node_Id;
      To   : Project_Node_Id)
   is
   begin
      pragma Assert
        (Node /= Empty_Node
          and then
            (Project_Nodes.Table (Node).Kind = N_With_Clause
               or else
             Project_Nodes.Table (Node).Kind = N_Variable_Reference
               or else
             Project_Nodes.Table (Node).Kind = N_Attribute_Reference));
      Project_Nodes.Table (Node).Field1 := To;
   end Set_Project_Node_Of;

   ---------------------------------------
   -- Set_Project_Of_Renamed_Package_Of --
   ---------------------------------------

   procedure Set_Project_Of_Renamed_Package_Of
     (Node : Project_Node_Id;
      To   : Project_Node_Id)
   is
   begin
      pragma Assert
        (Node /= Empty_Node
          and then
            Project_Nodes.Table (Node).Kind = N_Package_Declaration);
      Project_Nodes.Table (Node).Field1 := To;
   end Set_Project_Of_Renamed_Package_Of;

   ------------------------
   -- Set_String_Type_Of --
   ------------------------

   procedure Set_String_Type_Of
     (Node : Project_Node_Id;
      To   : Project_Node_Id)
   is
   begin
      pragma Assert
        (Node /= Empty_Node
          and then
            (Project_Nodes.Table (Node).Kind = N_Variable_Reference
               or else
             Project_Nodes.Table (Node).Kind = N_Typed_Variable_Declaration)
           and then
            Project_Nodes.Table (To).Kind    = N_String_Type_Declaration);

      if Project_Nodes.Table (Node).Kind = N_Variable_Reference then
         Project_Nodes.Table (Node).Field3 := To;
      else
         Project_Nodes.Table (Node).Field2 := To;
      end if;
   end Set_String_Type_Of;

   -------------------------
   -- Set_String_Value_Of --
   -------------------------

   procedure Set_String_Value_Of
     (Node : Project_Node_Id;
      To   : String_Id)
   is
   begin
      pragma Assert
        (Node /= Empty_Node
          and then
            (Project_Nodes.Table (Node).Kind = N_With_Clause
               or else
             Project_Nodes.Table (Node).Kind = N_Literal_String));
      Project_Nodes.Table (Node).Value := To;
   end Set_String_Value_Of;

   --------------------
   -- String_Type_Of --
   --------------------

   function String_Type_Of  (Node : Project_Node_Id)
                            return Project_Node_Id is
   begin
      pragma Assert
        (Node /= Empty_Node
          and then
            (Project_Nodes.Table (Node).Kind = N_Variable_Reference
               or else
             Project_Nodes.Table (Node).Kind = N_Typed_Variable_Declaration));

      if Project_Nodes.Table (Node).Kind = N_Variable_Reference then
         return Project_Nodes.Table (Node).Field3;
      else
         return Project_Nodes.Table (Node).Field2;
      end if;
   end String_Type_Of;

   ---------------------
   -- String_Value_Of --
   ---------------------

   function String_Value_Of (Node : Project_Node_Id) return String_Id is
   begin
      pragma Assert
        (Node /= Empty_Node
          and then
           (Project_Nodes.Table (Node).Kind = N_With_Clause
              or else
            Project_Nodes.Table (Node).Kind = N_Literal_String));
      return Project_Nodes.Table (Node).Value;
   end String_Value_Of;

   --------------------
   -- Value_Is_Valid --
   --------------------

   function Value_Is_Valid
     (For_Typed_Variable : Project_Node_Id;
      Value              : String_Id)
      return               Boolean
   is
   begin
      pragma Assert
        (For_Typed_Variable /= Empty_Node
          and then
           (Project_Nodes.Table (For_Typed_Variable).Kind =
                                     N_Typed_Variable_Declaration));

      declare
         Current_String : Project_Node_Id :=
                            First_Literal_String
                              (String_Type_Of (For_Typed_Variable));

      begin
         while Current_String /= Empty_Node
           and then
             not String_Equal (String_Value_Of (Current_String), Value)
         loop
            Current_String :=
              Next_Literal_String (Current_String);
         end loop;

         return Current_String /= Empty_Node;
      end;

   end Value_Is_Valid;

end Prj.Tree;
