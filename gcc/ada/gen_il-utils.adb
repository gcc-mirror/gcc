------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                          G E N _ I L . U T I L S                         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2020-2021, Free Software Foundation, Inc.         --
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

package body Gen_IL.Utils is

   procedure Nil (T : Node_Or_Entity_Type) is
   begin
      null;
   end Nil;

   function Node_Or_Entity (Root : Root_Type) return String is
   begin
      if Root = Node_Kind then
         return "Node";
      else
         return "Entity";
      end if;
   end Node_Or_Entity;

   function Num_Concrete_Descendants
     (T : Node_Or_Entity_Type) return Natural is
   begin
      return Concrete_Type'Pos (Type_Table (T).Last) -
        Concrete_Type'Pos (Type_Table (T).First) + 1;
   end Num_Concrete_Descendants;

   function First_Abstract (Root : Root_Type) return Abstract_Type is
     (case Root is
        when Node_Kind => Abstract_Node'First,
        when others => Abstract_Entity'First);  -- Entity_Kind
   function Last_Abstract (Root : Root_Type) return Abstract_Type is
     (case Root is
        when Node_Kind => Abstract_Node'Last,
        when others => Abstract_Entity'Last);  -- Entity_Kind

   function First_Concrete (Root : Root_Type) return Concrete_Type is
     (case Root is
        when Node_Kind => Concrete_Node'First,
        when others => Concrete_Entity'First);  -- Entity_Kind
   function Last_Concrete (Root : Root_Type) return Concrete_Type is
     (case Root is
        when Node_Kind => Concrete_Node'Last,
        when others => Concrete_Entity'Last);  -- Entity_Kind

   function First_Field (Root : Root_Type) return Field_Enum is
     (case Root is
        when Node_Kind => Node_Field'First,
        when others => Entity_Field'First);  -- Entity_Kind
   function Last_Field (Root : Root_Type) return Field_Enum is
     (case Root is
        when Node_Kind => Node_Field'Last,
        when others => Entity_Field'Last);  -- Entity_Kind
   --  First and Last node or entity fields

   procedure Verify_Type_Table is
   begin
      for T in Node_Or_Entity_Type loop
         if Type_Table (T) /= null then
            if not Type_Table (T).Is_Union then
               case T is
                  when Concrete_Node | Concrete_Entity =>
                     pragma Assert (Type_Table (T).First = T);
                     pragma Assert (Type_Table (T).Last = T);

                  when Abstract_Node | Abstract_Entity =>
                     pragma Assert
                       (Type_Table (T).First < Type_Table (T).Last);

                  when Boundaries =>
                     null;
               end case;
            end if;
         end if;
      end loop;
   end Verify_Type_Table;

   function Id_Image (T : Type_Enum) return String is
   begin
      case T is
         when Flag =>
            return "Boolean";
         when Node_Kind =>
            return "Node_Id";
         when Entity_Kind =>
            return "Entity_Id";
         when Nkind_Type =>
            return "Node_Kind";
         when Ekind_Type =>
            return "Entity_Kind";
         when others =>
            return Image (T) & "_Id";
      end case;
   end Id_Image;

   function Get_Set_Id_Image (T : Type_Enum) return String is
   begin
      case T is
         when Node_Kind =>
            return "Node_Id";
         when Entity_Kind =>
            return "Entity_Id";
         when Nkind_Type =>
            return "Node_Kind";
         when Ekind_Type =>
            return "Entity_Kind";
         when others =>
            return Image (T);
      end case;
   end Get_Set_Id_Image;

   function Image (T : Opt_Type_Enum) return String is
   begin
      case T is
         --  We special case the following; otherwise the compiler will give
         --  "wrong case" warnings in compiler code.

         when N_Pop_xxx_Label =>
            return "N_Pop_xxx_Label";

         when N_Push_Pop_xxx_Label =>
            return "N_Push_Pop_xxx_Label";

         when N_Push_xxx_Label =>
            return "N_Push_xxx_Label";

         when N_Raise_xxx_Error =>
            return "N_Raise_xxx_Error";

         when N_SCIL_Node =>
            return "N_SCIL_Node";

         when N_SCIL_Dispatch_Table_Tag_Init =>
            return "N_SCIL_Dispatch_Table_Tag_Init";

         when N_SCIL_Dispatching_Call =>
            return "N_SCIL_Dispatching_Call";

         when N_SCIL_Membership_Test =>
            return "N_SCIL_Membership_Test";

         when others =>
            return Capitalize (T'Img);
      end case;
   end Image;

   function Image_Sans_N (T : Opt_Type_Enum) return String is
      Im : constant String := Image (T);
      pragma Assert (Im (1 .. 2) = "N_");
   begin
      return Im (3 .. Im'Last);
   end Image_Sans_N;

   procedure Put_Images (S : in out Sink'Class; U : Type_Vector) is
      First_Time : Boolean := True;
   begin
      Indent (S, 3);

      for T of U loop
         if First_Time then
            First_Time := False;
         else
            Put (S, "\n| ");
         end if;

         Put (S, "\1", Image (T));
      end loop;

      Outdent (S, 3);
   end Put_Images;

   procedure Put_Id_Images (S : in out Sink'Class; U : Type_Vector) is
      First_Time : Boolean := True;
   begin
      Indent (S, 3);

      for T of U loop
         if First_Time then
            First_Time := False;
         else
            Put (S, "\n| ");
         end if;

         Put (S, "\1", Id_Image (T));
      end loop;

      Outdent (S, 3);
   end Put_Id_Images;

   function Image (F : Opt_Field_Enum) return String is
   begin
      case F is
         --  Special cases for the same reason as in the above Image
         --  function.

         when Alloc_For_BIP_Return =>
            return "Alloc_For_BIP_Return";
         when Assignment_OK =>
            return "Assignment_OK";
         when Backwards_OK =>
            return "Backwards_OK";
         when BIP_Initialization_Call =>
            return "BIP_Initialization_Call";
         when Body_Needed_For_SAL =>
            return "Body_Needed_For_SAL";
         when Conversion_OK =>
            return "Conversion_OK";
         when CR_Discriminant =>
            return "CR_Discriminant";
         when DTC_Entity =>
            return "DTC_Entity";
         when DT_Entry_Count =>
            return "DT_Entry_Count";
         when DT_Offset_To_Top_Func =>
            return "DT_Offset_To_Top_Func";
         when DT_Position =>
            return "DT_Position";
         when Forwards_OK =>
            return "Forwards_OK";
         when Has_Inherited_DIC =>
            return "Has_Inherited_DIC";
         when Has_Own_DIC =>
            return "Has_Own_DIC";
         when Has_RACW =>
            return "Has_RACW";
         when Has_SP_Choice =>
            return "Has_SP_Choice";
         when Ignore_SPARK_Mode_Pragmas =>
            return "Ignore_SPARK_Mode_Pragmas";
         when Is_Constr_Subt_For_UN_Aliased =>
            return "Is_Constr_Subt_For_UN_Aliased";
         when Is_CPP_Class =>
            return "Is_CPP_Class";
         when Is_CUDA_Kernel =>
            return "Is_CUDA_Kernel";
         when Is_DIC_Procedure =>
            return "Is_DIC_Procedure";
         when Is_Discrim_SO_Function =>
            return "Is_Discrim_SO_Function";
         when Is_Elaboration_Checks_OK_Id =>
            return "Is_Elaboration_Checks_OK_Id";
         when Is_Elaboration_Checks_OK_Node =>
            return "Is_Elaboration_Checks_OK_Node";
         when Is_Elaboration_Warnings_OK_Id =>
            return "Is_Elaboration_Warnings_OK_Id";
         when Is_Elaboration_Warnings_OK_Node =>
            return "Is_Elaboration_Warnings_OK_Node";
         when Is_Known_Guaranteed_ABE =>
            return "Is_Known_Guaranteed_ABE";
         when Is_RACW_Stub_Type =>
            return "Is_RACW_Stub_Type";
         when Is_SPARK_Mode_On_Node =>
            return "Is_SPARK_Mode_On_Node";
         when Local_Raise_Not_OK =>
            return "Local_Raise_Not_OK";
         when OK_To_Rename =>
            return "OK_To_Rename";
         when Referenced_As_LHS =>
            return "Referenced_As_LHS";
         when RM_Size =>
            return "RM_Size";
         when SCIL_Controlling_Tag =>
            return "SCIL_Controlling_Tag";
         when SCIL_Entity =>
            return "SCIL_Entity";
         when SCIL_Tag_Value =>
            return "SCIL_Tag_Value";
         when SCIL_Target_Prim =>
            return "SCIL_Target_Prim";
         when Shift_Count_OK =>
            return "Shift_Count_OK";
         when SPARK_Aux_Pragma =>
            return "SPARK_Aux_Pragma";
         when SPARK_Aux_Pragma_Inherited =>
            return "SPARK_Aux_Pragma_Inherited";
         when SPARK_Pragma =>
            return "SPARK_Pragma";
         when SPARK_Pragma_Inherited =>
            return "SPARK_Pragma_Inherited";
         when Split_PPC =>
            return "Split_PPC";
         when SSO_Set_High_By_Default =>
            return "SSO_Set_High_By_Default";
         when SSO_Set_Low_By_Default =>
            return "SSO_Set_Low_By_Default";
         when TSS_Elist =>
            return "TSS_Elist";

         when others =>
            return Capitalize (F'Img);
      end case;
   end Image;

   function Image (Default : Field_Default_Value) return String is
     (Capitalize (Default'Img));

   function Value_Image (Default : Field_Default_Value) return String is
   begin
      if Default = No_Default then
         return Image (Default);

      else
         --  Strip off the prefix and capitalize it

         declare
            Im : constant String := Image (Default);
            Prefix : constant String := "Default_";
         begin
            pragma Assert (Im (1 .. Prefix'Length) = Prefix);
            return Im (Prefix'Length + 1 .. Im'Last);
         end;
      end if;
   end Value_Image;

   procedure Iterate_Types
     (Root  : Node_Or_Entity_Type;
      Pre, Post : not null access procedure (T : Node_Or_Entity_Type) :=
        Nil'Access)
   is
      procedure Recursive (T : Node_Or_Entity_Type);
      --  Recursive walk

      procedure Recursive (T : Node_Or_Entity_Type) is
      begin
         Pre (T);

         for Child of Type_Table (T).Children loop
            Recursive (Child);
         end loop;

         Post (T);
      end Recursive;

   begin
      Recursive (Root);
   end Iterate_Types;

   function Is_Descendant (Ancestor, Descendant : Node_Or_Entity_Type)
     return Boolean is
   begin
      if Ancestor = Descendant then
         return True;

      elsif Descendant in Root_Type then
         return False;

      else
         return Is_Descendant (Ancestor, Type_Table (Descendant).Parent);
      end if;
   end Is_Descendant;

   procedure Put_Type_Hierarchy (S : in out Sink'Class; Root : Root_Type) is
      Level : Natural := 0;

      function Indentation return String is ((1 .. 3 * Level => ' '));
      --  Indentation string of space characters. We can't use the Indent
      --  primitive, because we want this indentation after the "--".

      procedure Pre (T : Node_Or_Entity_Type);
      procedure Post (T : Node_Or_Entity_Type);
      --  Pre and Post actions passed to Iterate_Types

      procedure Pre (T : Node_Or_Entity_Type) is
      begin
         if not Type_Table (T).Allow_Overlap then
            Put (S, "--  \1\2\n", Indentation, Image (T));
         end if;

         Level := Level + 1;
      end Pre;

      procedure Post (T : Node_Or_Entity_Type) is
      begin
         Level := Level - 1;

         if not Type_Table (T).Allow_Overlap then
            --  Put out an "end" line only if there are many descendants, for
            --  an arbitrary definition of "many".

            if Num_Concrete_Descendants (T) > 10 then
               Put (S, "--  \1end \2\n", Indentation, Image (T));
            end if;
         end if;
      end Post;

      N_Or_E : constant String :=
        (case Root is
           when Node_Kind => "nodes",
           when others => "entities");  -- Entity_Kind

   begin
      Put (S, "--  Type hierarchy for \1\n", N_Or_E);
      Put (S, "--\n");

      Iterate_Types (Root, Pre'Access, Post'Access);

      Put (S, "--\n");
      Put (S, "--  End type hierarchy for \1\n\n", N_Or_E);
   end Put_Type_Hierarchy;

   function Pos (T : Concrete_Type) return Root_Nat is
      First : constant Concrete_Type :=
        (if T in Concrete_Node then Concrete_Node'First
         else Concrete_Entity'First);
   begin
      return Type_Enum'Pos (T) - Type_Enum'Pos (First);
   end Pos;

   Stdout : Sink'Class renames Files.Standard_Output.all;

   --  The following procedures are for use in gdb. They use the 'Put_Image
   --  attribute. That is commented out, because we don't want this new feature
   --  used in the compiler. If you need this for debugging, just uncomment
   --  those lines back in, and rebuild.

   pragma Warnings (Off);
   procedure Ptypes (V : Type_Vector) is
   begin
--      Type_Vector'Put_Image (Stdout, V);
      New_Line (Stdout);
      Flush (Stdout);
   end Ptypes;

   procedure Pfields (V : Field_Vector) is
   begin
--      Field_Vector'Put_Image (Stdout, V);
      New_Line (Stdout);
      Flush (Stdout);
   end Pfields;
   pragma Warnings (On);

end Gen_IL.Utils;
