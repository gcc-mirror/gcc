------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                U N A M E                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2004, Free Software Foundation, Inc.         --
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
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Atree;    use Atree;
with Casing;   use Casing;
with Einfo;    use Einfo;
with Hostparm;
with Lib;      use Lib;
with Namet;    use Namet;
with Nlists;   use Nlists;
with Output;   use Output;
with Sinfo;    use Sinfo;
with Sinput;   use Sinput;

package body Uname is

   -------------------
   -- Get_Body_Name --
   -------------------

   function Get_Body_Name (N : Unit_Name_Type) return Unit_Name_Type is
   begin
      Get_Name_String (N);

      pragma Assert (Name_Len > 2
                       and then Name_Buffer (Name_Len - 1) = '%'
                       and then Name_Buffer (Name_Len) = 's');

      Name_Buffer (Name_Len) := 'b';
      return Name_Find;
   end Get_Body_Name;

   -----------------------------------
   -- Get_External_Unit_Name_String --
   -----------------------------------

   procedure Get_External_Unit_Name_String (N : Unit_Name_Type) is
      Pcount : Natural;
      Newlen : Natural;

   begin
      --  Get unit name and eliminate trailing %s or %b

      Get_Name_String (N);
      Name_Len := Name_Len - 2;

      --  Find number of components

      Pcount := 0;
      for J in 1 .. Name_Len loop
         if Name_Buffer (J) = '.' then
            Pcount := Pcount + 1;
         end if;
      end loop;

      --  If simple name, nothing to do

      if Pcount = 0 then
         return;
      end if;

      --  If name has multiple components, replace dots by double underscore

      Newlen := Name_Len + Pcount;

      for J in reverse 1 .. Name_Len loop
         if Name_Buffer (J) = '.' then
            Name_Buffer (Newlen) := '_';
            Name_Buffer (Newlen - 1) := '_';
            Newlen := Newlen - 2;

         else
            Name_Buffer (Newlen) := Name_Buffer (J);
            Newlen := Newlen - 1;
         end if;
      end loop;

      Name_Len := Name_Len + Pcount;
   end Get_External_Unit_Name_String;

   --------------------------
   -- Get_Parent_Body_Name --
   --------------------------

   function Get_Parent_Body_Name (N : Unit_Name_Type) return Unit_Name_Type is
   begin
      Get_Name_String (N);

      while Name_Buffer (Name_Len) /= '.' loop
         pragma Assert (Name_Len > 1); -- not a child or subunit name
         Name_Len := Name_Len - 1;
      end loop;

      Name_Buffer (Name_Len) := '%';
      Name_Len := Name_Len + 1;
      Name_Buffer (Name_Len) := 'b';
      return Name_Find;

   end Get_Parent_Body_Name;

   --------------------------
   -- Get_Parent_Spec_Name --
   --------------------------

   function Get_Parent_Spec_Name (N : Unit_Name_Type) return Unit_Name_Type is
   begin
      Get_Name_String (N);

      while Name_Buffer (Name_Len) /= '.' loop
         if Name_Len = 1 then
            return No_Name; -- not a child or subunit name
         else
            Name_Len := Name_Len - 1;
         end if;
      end loop;

      Name_Buffer (Name_Len) := '%';
      Name_Len := Name_Len + 1;
      Name_Buffer (Name_Len) := 's';
      return Name_Find;

   end Get_Parent_Spec_Name;

   -------------------
   -- Get_Spec_Name --
   -------------------

   function Get_Spec_Name (N : Unit_Name_Type) return Unit_Name_Type is
   begin
      Get_Name_String (N);

      pragma Assert (Name_Len > 2
                       and then Name_Buffer (Name_Len - 1) = '%'
                       and then Name_Buffer (Name_Len) = 'b');

      Name_Buffer (Name_Len) := 's';
      return Name_Find;
   end Get_Spec_Name;

   -------------------
   -- Get_Unit_Name --
   -------------------

   function Get_Unit_Name (N : Node_Id) return Unit_Name_Type is

      Unit_Name_Buffer : String (1 .. Hostparm.Max_Name_Length);
      --  Buffer used to build name of unit. Note that we cannot use the
      --  Name_Buffer in package Name_Table because we use it to read
      --  component names.

      Unit_Name_Length : Natural := 0;
      --  Length of name stored in Unit_Name_Buffer

      Node : Node_Id;
      --  Program unit node

      procedure Add_Char (C : Character);
      --  Add a single character to stored unit name

      procedure Add_Name (Name : Name_Id);
      --  Add the characters of a names table entry to stored unit name

      procedure Add_Node_Name (Node : Node_Id);
      --  Recursive procedure adds characters associated with Node

      function Get_Parent (Node : Node_Id) return Node_Id;
      --  Get parent compilation unit of a stub

      --------------
      -- Add_Char --
      --------------

      procedure Add_Char (C : Character) is
      begin
         --  Should really check for max length exceeded here???
         Unit_Name_Length := Unit_Name_Length + 1;
         Unit_Name_Buffer (Unit_Name_Length) := C;
      end Add_Char;

      --------------
      -- Add_Name --
      --------------

      procedure Add_Name (Name : Name_Id) is
      begin
         Get_Name_String (Name);

         for J in 1 .. Name_Len loop
            Add_Char (Name_Buffer (J));
         end loop;
      end Add_Name;

      -------------------
      -- Add_Node_Name --
      -------------------

      procedure Add_Node_Name (Node : Node_Id) is
         Kind : constant Node_Kind := Nkind (Node);

      begin
         --  Just ignore an error node (someone else will give a message)

         if Node = Error then
            return;

         --  Otherwise see what kind of node we have

         else
            case Kind is

               when N_Identifier                      |
                    N_Defining_Identifier             |
                    N_Defining_Operator_Symbol        =>

                  --  Note: it is of course an error to have a defining
                  --  operator symbol at this point, but this is not where
                  --  the error is signalled, so we handle it nicely here!

                  Add_Name (Chars (Node));

               when N_Defining_Program_Unit_Name      =>
                  Add_Node_Name (Name (Node));
                  Add_Char ('.');
                  Add_Node_Name (Defining_Identifier (Node));

               when N_Selected_Component              |
                    N_Expanded_Name                   =>
                  Add_Node_Name (Prefix (Node));
                  Add_Char ('.');
                  Add_Node_Name (Selector_Name (Node));

               when N_Subprogram_Specification        |
                    N_Package_Specification           =>
                  Add_Node_Name (Defining_Unit_Name (Node));

               when N_Subprogram_Body                 |
                    N_Subprogram_Declaration          |
                    N_Package_Declaration             |
                    N_Generic_Declaration             =>
                  Add_Node_Name (Specification (Node));

               when N_Generic_Instantiation           =>
                  Add_Node_Name (Defining_Unit_Name (Node));

               when N_Package_Body                    =>
                  Add_Node_Name (Defining_Unit_Name (Node));

               when N_Task_Body                       |
                    N_Protected_Body                  =>
                  Add_Node_Name (Defining_Identifier (Node));

               when N_Package_Renaming_Declaration    =>
                  Add_Node_Name (Defining_Unit_Name (Node));

               when N_Subprogram_Renaming_Declaration =>
                  Add_Node_Name (Specification (Node));

               when N_Generic_Renaming_Declaration   =>
                  Add_Node_Name (Defining_Unit_Name (Node));

               when N_Subprogram_Body_Stub            =>
                  Add_Node_Name (Get_Parent (Node));
                  Add_Char ('.');
                  Add_Node_Name (Specification (Node));

               when N_Compilation_Unit                =>
                  Add_Node_Name (Unit (Node));

               when N_Package_Body_Stub               =>
                  Add_Node_Name (Get_Parent (Node));
                  Add_Char ('.');
                  Add_Node_Name (Defining_Identifier (Node));

               when N_Task_Body_Stub                  |
                    N_Protected_Body_Stub             =>
                  Add_Node_Name (Get_Parent (Node));
                  Add_Char ('.');
                  Add_Node_Name (Defining_Identifier (Node));

               when N_Subunit                         =>
                  Add_Node_Name (Name (Node));
                  Add_Char ('.');
                  Add_Node_Name (Proper_Body (Node));

               when N_With_Clause                     =>
                  Add_Node_Name (Name (Node));

               when N_Pragma                          =>
                  Add_Node_Name (Expression (First
                    (Pragma_Argument_Associations (Node))));

               --  Tasks and protected stuff appear only in an error context,
               --  but the error has been posted elsewhere, so we deal nicely
               --  with these error situations here, and produce a reasonable
               --  unit name using the defining identifier.

               when N_Task_Type_Declaration           |
                    N_Single_Task_Declaration         |
                    N_Protected_Type_Declaration      |
                    N_Single_Protected_Declaration    =>
                  Add_Node_Name (Defining_Identifier (Node));

               when others =>
                  raise Program_Error;

            end case;
         end if;
      end Add_Node_Name;

      ----------------
      -- Get_Parent --
      ----------------

      function Get_Parent (Node : Node_Id) return Node_Id is
         N : Node_Id := Node;

      begin
         while Nkind (N) /= N_Compilation_Unit loop
            N := Parent (N);
         end loop;

         return N;
      end Get_Parent;

   -------------------------------------------
   -- Start of Processing for Get_Unit_Name --
   -------------------------------------------

   begin
      Node := N;

      --  If we have Defining_Identifier, find the associated unit node

      if Nkind (Node) = N_Defining_Identifier then
         Node := Declaration_Node (Node);

      --  If an expanded name, it is an already analyzed child unit, find
      --  unit node.

      elsif Nkind (Node) = N_Expanded_Name then
         Node := Declaration_Node (Entity (Node));
      end if;

      if Nkind (Node) = N_Package_Specification
        or else Nkind (Node) in N_Subprogram_Specification
      then
         Node := Parent (Node);
      end if;

      --  Node points to the unit, so get its name and add proper suffix

      Add_Node_Name (Node);
      Add_Char ('%');

      case Nkind (Node) is
         when N_Generic_Declaration             |
              N_Subprogram_Declaration          |
              N_Package_Declaration             |
              N_With_Clause                     |
              N_Pragma                          |
              N_Generic_Instantiation           |
              N_Package_Renaming_Declaration    |
              N_Subprogram_Renaming_Declaration |
              N_Generic_Renaming_Declaration    |
              N_Single_Task_Declaration         |
              N_Single_Protected_Declaration    |
              N_Task_Type_Declaration           |
              N_Protected_Type_Declaration      =>

            Add_Char ('s');

         when N_Subprogram_Body                 |
              N_Package_Body                    |
              N_Subunit                         |
              N_Body_Stub                       |
              N_Task_Body                       |
              N_Protected_Body                  |
              N_Identifier                      |
              N_Selected_Component              =>

            Add_Char ('b');

         when others =>
            raise Program_Error;
      end case;

      Name_Buffer (1 .. Unit_Name_Length) :=
        Unit_Name_Buffer (1 .. Unit_Name_Length);
      Name_Len := Unit_Name_Length;
      return Name_Find;

   end Get_Unit_Name;

   --------------------------
   -- Get_Unit_Name_String --
   --------------------------

   procedure Get_Unit_Name_String (N : Unit_Name_Type) is
      Unit_Is_Body : Boolean;

   begin
      Get_Decoded_Name_String (N);
      Unit_Is_Body := Name_Buffer (Name_Len) = 'b';
      Set_Casing (Identifier_Casing (Source_Index (Main_Unit)), Mixed_Case);

      --  A special fudge, normally we don't have operator symbols present,
      --  since it is always an error to do so. However, if we do, at this
      --  stage it has the form:

      --    "and"

      --  and the %s or %b has already been eliminated so put 2 chars back

      if Name_Buffer (1) = '"' then
         Name_Len := Name_Len + 2;
      end if;

      --  Now adjust the %s or %b to (spec) or (body)

      if Unit_Is_Body then
         Name_Buffer (Name_Len - 1 .. Name_Len + 5) := " (body)";
      else
         Name_Buffer (Name_Len - 1 .. Name_Len + 5) := " (spec)";
      end if;

      for J in 1 .. Name_Len loop
         if Name_Buffer (J) = '-' then
            Name_Buffer (J) := '.';
         end if;
      end loop;

      Name_Len := Name_Len + (7 - 2);
   end Get_Unit_Name_String;

   ------------------
   -- Is_Body_Name --
   ------------------

   function Is_Body_Name (N : Unit_Name_Type) return Boolean is
   begin
      Get_Name_String (N);
      return Name_Len > 2
        and then Name_Buffer (Name_Len - 1) = '%'
        and then Name_Buffer (Name_Len) = 'b';
   end Is_Body_Name;

   -------------------
   -- Is_Child_Name --
   -------------------

   function Is_Child_Name (N : Unit_Name_Type) return Boolean is
      J : Natural;

   begin
      Get_Name_String (N);
      J := Name_Len;

      while Name_Buffer (J) /= '.' loop
         if J = 1 then
            return False; -- not a child or subunit name
         else
            J := J - 1;
         end if;
      end loop;

      return True;
   end Is_Child_Name;

   ------------------
   -- Is_Spec_Name --
   ------------------

   function Is_Spec_Name (N : Unit_Name_Type) return Boolean is
   begin
      Get_Name_String (N);
      return Name_Len > 2
        and then Name_Buffer (Name_Len - 1) = '%'
        and then Name_Buffer (Name_Len) = 's';
   end Is_Spec_Name;

   -----------------------
   -- Name_To_Unit_Name --
   -----------------------

   function Name_To_Unit_Name (N : Name_Id) return Unit_Name_Type is
   begin
      Get_Name_String (N);
      Name_Buffer (Name_Len + 1) := '%';
      Name_Buffer (Name_Len + 2) := 's';
      Name_Len := Name_Len + 2;
      return Name_Find;
   end Name_To_Unit_Name;

   ---------------
   -- New_Child --
   ---------------

   function New_Child
     (Old  : Unit_Name_Type;
      Newp : Unit_Name_Type)
      return Unit_Name_Type
   is
      P : Natural;

   begin
      Get_Name_String (Old);

      declare
         Child : constant String := Name_Buffer (1 .. Name_Len);

      begin
         Get_Name_String (Newp);
         Name_Len := Name_Len - 2;

         P := Child'Last;
         while Child (P) /= '.' loop
            P := P - 1;
         end loop;

         while P <= Child'Last loop
            Name_Len := Name_Len + 1;
            Name_Buffer (Name_Len) := Child (P);
            P := P + 1;
         end loop;

         return Name_Find;
      end;
   end New_Child;

   --------------
   -- Uname_Ge --
   --------------

   function Uname_Ge (Left, Right : Unit_Name_Type) return Boolean is
   begin
      return Left = Right or else Uname_Gt (Left, Right);
   end Uname_Ge;

   --------------
   -- Uname_Gt --
   --------------

   function Uname_Gt (Left, Right : Unit_Name_Type) return Boolean is
   begin
      return Left /= Right and then not Uname_Lt (Left, Right);
   end Uname_Gt;

   --------------
   -- Uname_Le --
   --------------

   function Uname_Le (Left, Right : Unit_Name_Type) return Boolean is
   begin
      return Left = Right or else Uname_Lt (Left, Right);
   end Uname_Le;

   --------------
   -- Uname_Lt --
   --------------

   function Uname_Lt (Left, Right : Unit_Name_Type) return Boolean is
      Left_Name    : String (1 .. Hostparm.Max_Name_Length);
      Left_Length  : Natural;
      Right_Name   : String renames Name_Buffer;
      Right_Length : Natural renames Name_Len;
      J            : Natural;

   begin
      pragma Warnings (Off, Right_Length);
      --  Suppress warnings on Right_Length, used in pragma Assert

      if Left = Right then
         return False;
      end if;

      Get_Name_String (Left);
      Left_Name  (1 .. Name_Len + 1) := Name_Buffer (1 .. Name_Len + 1);
      Left_Length := Name_Len;
      Get_Name_String (Right);
      J := 1;

      loop
         exit when Left_Name (J) = '%';

         if Right_Name (J) = '%' then
            return False; -- left name is longer
         end if;

         pragma Assert (J <= Left_Length and then J <= Right_Length);

         if Left_Name (J) /= Right_Name (J) then
            return Left_Name (J) < Right_Name (J); -- parent names different
         end if;

         J := J + 1;
      end loop;

      --  Come here pointing to % in left name

      if Right_Name (J) /= '%' then
         return True; -- right name is longer
      end if;

      --  Here the parent names are the same and specs sort low. If neither is
      --  a spec, then we are comparing the same name and we want a result of
      --  False in any case.

      return Left_Name (J + 1) = 's';
   end Uname_Lt;

   ---------------------
   -- Write_Unit_Name --
   ---------------------

   procedure Write_Unit_Name (N : Unit_Name_Type) is
   begin
      Get_Unit_Name_String (N);
      Write_Str (Name_Buffer (1 .. Name_Len));
   end Write_Unit_Name;

end Uname;
