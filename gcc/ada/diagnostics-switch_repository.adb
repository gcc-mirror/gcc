------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--  D I A G N O S T I C S . D I A G N O S T I C S _ R E P O S I T O R Y     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2024, Free Software Foundation, Inc.         --
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
with Diagnostics.JSON_Utils; use Diagnostics.JSON_Utils;
with Output;                 use Output;
package body Diagnostics.Switch_Repository is

   Switches : constant array (Switch_Id)
     of Switch_Type :=
     (No_Switch_Id            =>
        (others => <>),
      gnatwb                  =>
        (Human_Id          => new String'("Warn_On_Bad_Fixed_Value"),
         Status            => Active,
         Short_Name        => new String'("gnatwb"),
         Description       => null,
         Documentation_Url => null),
      gnatwc                  =>
        (Human_Id          => new String'("Constant_Condition_Warnings"),
         Status            => Active,
         Short_Name        => new String'("gnatwc"),
         Description       => null,
         Documentation_Url => null),
      gnatwd                  =>
        --  TODO: is this a subcheck of general gnatwu?
        (Human_Id          => new String'("Warn_On_Dereference"),
         Status            => Active,
         Short_Name        => new String'("gnatwd"),
         Description       => null,
         Documentation_Url => null),
      gnatwf                       =>
        (Human_Id          => new String'("Check_Unreferenced_Formals"),
         Status            => Active,
         Short_Name        => new String'("gnatwf"),
         Description       => null,
         Documentation_Url => null),
      gnatwg                       =>
        (Human_Id          => new String'("Warn_On_Unrecognized_Pragma"),
         Status            => Active,
         Short_Name        => new String'("gnatwg"),
         Description       => null,
         Documentation_Url => null),
      gnatwh                       =>
        (Human_Id          => new String'("Warn_On_Hiding"),
         Status            => Active,
         Short_Name        => new String'("gnatwh"),
         Description       => null,
         Documentation_Url => null),
      gnatwi                       =>
        (Human_Id          => new String'("Implementation_Unit_Warnings"),
         Status            => Active,
         Short_Name        => new String'("gnatwi"),
         Description       => null,
         Documentation_Url => null),
      gnatwj                      =>
        (Human_Id          => new String'("Warn_On_Obsolescent_Feature"),
         Status            => Active,
         Short_Name        => new String'("gnatwj"),
         Description       => null,
         Documentation_Url => null),
      gnatwk                      =>
        (Human_Id          => new String'("Warn_On_Constant"),
         Status            => Active,
         Short_Name        => new String'("gnatwk"),
         Description       => null,
         Documentation_Url => null),
      gnatwl                      =>
        (Human_Id          => new String'("Elab_Warnings"),
         Status            => Active,
         Short_Name        => new String'("gnatwl"),
         Description       => null,
         Documentation_Url => null),
      gnatwm                      =>
        (Human_Id          => new String'("Warn_On_Modified_Unread"),
         Status            => Active,
         Short_Name        => new String'("gnatwm"),
         Description       => null,
         Documentation_Url => null),
      gnatwo                      =>
        (Human_Id          => new String'("Address_Clause_Overlay_Warnings"),
         Status            => Active,
         Short_Name        => new String'("gnatwo"),
         Description       => null,
         Documentation_Url => null),
      gnatwp                      =>
        (Human_Id          => new String'("Ineffective_Inline_Warnings"),
         Status            => Active,
         Short_Name        => new String'("gnatwp"),
         Description       => null,
         Documentation_Url => null),
      gnatwq                      =>
        (Human_Id => new String'("Warn_On_Questionable_Missing_Parens"),
         Status            => Active,
         Short_Name        => new String'("gnatwq"),
         Description       => null,
         Documentation_Url => null),
      gnatwr                      =>
        (Human_Id          => new String'("Warn_On_Redundant_Constructs"),
         Status            => Active,
         Short_Name        => new String'("gnatwr"),
         Description       => null,
         Documentation_Url => null),
      gnatwt                      =>
        (Human_Id          => new String'("Warn_On_Deleted_Code"),
         Status            => Active,
         Short_Name        => new String'("gnatwt"),
         Description       => null,
         Documentation_Url => null),
      gnatwu                      =>
        (Human_Id          => new String'("Warn_On_Unused_Entities"),
         Status            => Active,
         Short_Name        => new String'("gnatwu"),
         Description       => null,
         Documentation_Url => null),
      gnatwv                      =>
        (Human_Id          => new String'("Warn_On_No_Value_Assigned"),
         Status            => Active,
         Short_Name        => new String'("gnatwv"),
         Description       => null,
         Documentation_Url => null),
      gnatww                      =>
        (Human_Id          => new String'("Warn_On_Assumed_Low_Bound"),
         Status            => Active,
         Short_Name        => new String'("gnatww"),
         Description       => null,
         Documentation_Url => null),
      gnatwx                      =>
        (Human_Id          => new String'("Warn_On_Export_Import"),
         Status            => Active,
         Short_Name        => new String'("gnatwx"),
         Description       => null,
         Documentation_Url => null),
      gnatwy                      =>
        (Human_Id          => new String'("Warn_On_Ada_Compatibility_Issues"),
         Status            => Active,
         Short_Name        => new String'("gnatwy"),
         Description       => null,
         Documentation_Url => null),
      gnatwz                      =>
        (Human_Id          => new String'("Warn_On_Unchecked_Conversion"),
         Status            => Active,
         Short_Name        => new String'("gnatwz"),
         Description       => null,
         Documentation_Url => null),
      gnatw_dot_a                      =>
        (Human_Id          => new String'("Warn_On_Assertion_Failure"),
         Status            => Active,
         Short_Name        => new String'("gnatw.a"),
         Description       => null,
         Documentation_Url => null),
      gnatw_dot_b                      =>
        (Human_Id          => new String'("Warn_On_Biased_Representation"),
         Status            => Active,
         Short_Name        => new String'("gnatw.b"),
         Description       => null,
         Documentation_Url => null),
      gnatw_dot_c                      =>
        (Human_Id          => new String'("Warn_On_Unrepped_Components"),
         Status            => Active,
         Short_Name        => new String'("gnatw.c"),
         Description       => null,
         Documentation_Url => null),
      gnatw_dot_f                      =>
        (Human_Id          => new String'("Warn_On_Elab_Access"),
         Status            => Active,
         Short_Name        => new String'("gnatw.f"),
         Description       => null,
         Documentation_Url => null),
      gnatw_dot_h                      =>
        (Human_Id          => new String'("Warn_On_Record_Holes"),
         Status            => Active,
         Short_Name        => new String'("gnatw.h"),
         Description       => null,
         Documentation_Url => null),
      gnatw_dot_i                      =>
        (Human_Id          => new String'("Warn_On_Overlap"),
         Status            => Active,
         Short_Name        => new String'("gnatw.i"),
         Description       => null,
         Documentation_Url => null),
      gnatw_dot_j                      =>
        (Human_Id          => new String'("Warn_On_Late_Primitives"),
         Status            => Active,
         Short_Name        => new String'("gnatw.j"),
         Description       => null,
         Documentation_Url => null),
      gnatw_dot_k                      =>
        (Human_Id          => new String'("Warn_On_Standard_Redefinition"),
         Status            => Active,
         Short_Name        => new String'("gnatw.k"),
         Description       => null,
         Documentation_Url => null),
      gnatw_dot_l                      =>
        (Human_Id          => new String'("List_Inherited_Aspects"),
         Status            => Active,
         Short_Name        => new String'("gnatw.l"),
         Description       => null,
         Documentation_Url => null),
      gnatw_dot_m                      =>
        (Human_Id          => new String'("Warn_On_Suspicious_Modulus_Value"),
         Status            => Active,
         Short_Name        => new String'("gnatw.m"),
         Description       => null,
         Documentation_Url => null),
      gnatw_dot_n                      =>
        (Human_Id          => new String'("Warn_On_Atomic_Synchronization"),
         Status            => Active,
         Short_Name        => new String'("gnatw.n"),
         Description       => null,
         Documentation_Url => null),
      gnatw_dot_o                      =>
        (Human_Id          => new String'("Warn_On_All_Unread_Out_Parameters"),
         Status            => Active,
         Short_Name        => new String'("gnatw.o"),
         Description       => null,
         Documentation_Url => null),
      gnatw_dot_p                      =>
        (Human_Id          => new String'("Warn_On_Parameter_Order"),
         Status            => Active,
         Short_Name        => new String'("gnatw.p"),
         Description       => null,
         Documentation_Url => null),
      gnatw_dot_q                      =>
        (Human_Id          => new String'("Warn_On_Questionable_Layout"),
         Status            => Active,
         Short_Name        => new String'("gnatw.q"),
         Description       => null,
         Documentation_Url => null),
      gnatw_dot_r                      =>
        (Human_Id          => new String'("Warn_On_Object_Renames_Function"),
         Status            => Active,
         Short_Name        => new String'("gnatw.r"),
         Description       => null,
         Documentation_Url => null),
      gnatw_dot_s                      =>
        (Human_Id          => new String'("Warn_On_Overridden_Size"),
         Status            => Active,
         Short_Name        => new String'("gnatw.s"),
         Description       => null,
         Documentation_Url => null),
      gnatw_dot_t                      =>
        (Human_Id          => new String'("Warn_On_Suspicious_Contract"),
         Status            => Active,
         Short_Name        => new String'("gnatw.t"),
         Description       => null,
         Documentation_Url => null),
      gnatw_dot_u                      =>
        (Human_Id => new String'("Warn_On_Unordered_Enumeration_Type"),
         Status            => Active,
         Short_Name        => new String'("gnatw.u"),
         Description       => null,
         Documentation_Url => null),
      gnatw_dot_v                      =>
        (Human_Id          => new String'("Warn_On_Reverse_Bit_Order"),
         Status            => Active,
         Short_Name        => new String'("gnatw.v"),
         Description       => null,
         Documentation_Url => null),
      gnatw_dot_w                      =>
        (Human_Id          => new String'("Warn_On_Warnings_Off"),
         Status            => Active,
         Short_Name        => new String'("gnatw.w"),
         Description       => null,
         Documentation_Url => null),
      gnatw_dot_x                      =>
        (Human_Id          =>
          new String'("Warn_No_Exception_Propagation_Active"),
         Status            => Active,
         Short_Name        => new String'("gnatw.x"),
         Description       => null,
         Documentation_Url => null),
      gnatw_dot_y                      =>
        (Human_Id          => new String'("List_Body_Required_Info"),
         Status            => Active,
         Short_Name        => new String'("gnatw.y"),
         Description       => null,
         Documentation_Url => null),
      gnatw_dot_z                      =>
        (Human_Id          => new String'("Warn_On_Size_Alignment"),
         Status            => Active,
         Short_Name        => new String'("gnatw.z"),
         Description       => null,
         Documentation_Url => null),
      gnatw_underscore_a                      =>
        (Human_Id          => new String'("Warn_On_Anonymous_Allocators"),
         Status            => Active,
         Short_Name        => new String'("gnatw_a"),
         Description       => null,
         Documentation_Url => null),
      gnatw_underscore_c                      =>
        (Human_Id => new String'("Warn_On_Unknown_Compile_Time_Warning"),
         Status            => Active,
         Short_Name        => new String'("gnatw_c"),
         Description       => null,
         Documentation_Url => null),
      gnatw_underscore_j                      =>
        (Human_Id => new String'("Warn_On_Non_Dispatching_Primitives"),
         Status            => Active,
         Short_Name        => new String'("gnatw_j"),
         Description       => null,
         Documentation_Url => null),
      gnatw_underscore_l                      =>
        (Human_Id => new String'("Warn_On_Inherently_Limited_Types"),
         Status            => Active,
         Short_Name        => new String'("gnatw_l"),
         Description       => null,
         Documentation_Url => null),
      gnatw_underscore_p                      =>
        (Human_Id          => new String'("Warn_On_Pedantic_Checks"),
         Status            => Active,
         Short_Name        => new String'("gnatw_p"),
         Description       => null,
         Documentation_Url => null),
      gnatw_underscore_q                      =>
        (Human_Id          => new String'("Warn_On_Ignored_Equality"),
         Status            => Active,
         Short_Name        => new String'("gnatw_q"),
         Description       => null,
         Documentation_Url => null),
      gnatw_underscore_r                      =>
        (Human_Id          => new String'("Warn_On_Component_Order"),
         Status            => Active,
         Short_Name        => new String'("gnatw_r"),
         Description       => null,
         Documentation_Url => null),
      gnatw_underscore_s                      =>
        (Human_Id => new String'("Warn_On_Ineffective_Predicate_Test"),
         Status            => Active,
         Short_Name        => new String'("gnatw_s"),
         Description       => null,
         Documentation_Url => null),
      --  NOTE: this flag is usually followed by a number specfifying the
      --  indentation level. We encode all of these warnings as -gnaty0
      --  irregardless of the actual numeric value.
      gnaty      =>
        (Human_Id          => new String'("Style_Check_Indentation_Level"),
         Status            => Active,
         Short_Name        => new String'("gnaty0"),
         Description       => null,
         Documentation_Url => null),
      gnatya  =>
        (Human_Id          => new String'("Style_Check_Attribute_Casing"),
         Status            => Active,
         Short_Name        => new String'("gnatya"),
         Description       => null,
         Documentation_Url => null),
      gnatyaa  =>
        (Human_Id          => new String'("Address_Clause_Overlay_Warnings"),
         Status            => Active,
         Short_Name        => new String'("gnatyA"),
         Description       => null,
         Documentation_Url => null),
      gnatyb  =>
        (Human_Id          => new String'("Style_Check_Blanks_At_End"),
         Status            => Active,
         Short_Name        => new String'("gnatyb"),
         Description       => null,
         Documentation_Url => null),
      gnatybb  =>
        --  NOTE: in live documentation it is called "Check Boolean operators"
        (Human_Id          => new String'("Style_Check_Boolean_And_Or"),
         Status            => Active,
         Short_Name        => new String'("gnatyB"),
         Description       => null,
         Documentation_Url => null),
      gnatyc  =>
        (Human_Id          => new String'("Style_Check_Comments_Double_Space"),
         Status            => Active,
         Short_Name        => new String'("gnatyc"),
         Description       => null,
         Documentation_Url => null),
      gnatycc  =>
        (Human_Id          => new String'("Style_Check_Comments_Single_Space"),
         Status            => Active,
         Short_Name        => new String'("gnatyC"),
         Description       => null,
         Documentation_Url => null),
      gnatyd  =>
        (Human_Id          => new String'("Style_Check_DOS_Line_Terminator"),
         Status            => Active,
         Short_Name        => new String'("gnatyd"),
         Description       => null,
         Documentation_Url => null),
      gnatydd  =>
        (Human_Id          => new String'("Style_Check_Mixed_Case_Decls"),
         Status            => Active,
         Short_Name        => new String'("gnatyD"),
         Description       => null,
         Documentation_Url => null),
      gnatye  =>
        (Human_Id          => new String'("Style_Check_End_Labels"),
         Status            => Active,
         Short_Name        => new String'("gnatye"),
         Description       => null,
         Documentation_Url => null),
      gnatyf =>
        (Human_Id          => new String'("Style_Check_Form_Feeds"),
         Status            => Active,
         Short_Name        => new String'("gnatyf"),
         Description       => null,
         Documentation_Url => null),
      gnatyh =>
        (Human_Id          => new String'("Style_Check_Horizontal_Tabs"),
         Status            => Active,
         Short_Name        => new String'("gnatyh"),
         Description       => null,
         Documentation_Url => null),
      gnatyi =>
        (Human_Id          => new String'("Style_Check_If_Then_Layout"),
         Status            => Active,
         Short_Name        => new String'("gnatyi"),
         Description       => null,
         Documentation_Url => null),
      gnatyii =>
        (Human_Id          => new String'("Style_Check_Mode_In"),
         Status            => Active,
         Short_Name        => new String'("gnatyI"),
         Description       => null,
         Documentation_Url => null),
      gnatyk =>
        (Human_Id          => new String'("Style_Check_Keyword_Casing"),
         Status            => Active,
         Short_Name        => new String'("gnatyk"),
         Description       => null,
         Documentation_Url => null),
      gnatyl =>
        (Human_Id          => new String'("Style_Check_Layout"),
         Status            => Active,
         Short_Name        => new String'("gnatyl"),
         Description       => null,
         Documentation_Url => null),
      gnatyll =>
        (Human_Id          => new String'("Style_Check_Max_Nesting_Level"),
         Status            => Active,
         Short_Name        => new String'("gnatyL"),
         Description       => null,
         Documentation_Url => null),
      gnatym =>
        (Human_Id          => new String'("Style_Check_Max_Line_Length"),
         Status            => Active,
         Short_Name        => new String'("gnatym"),
         Description       => null,
         Documentation_Url => null),
      gnatymm =>
        --  TODO: May contain line length
        (Human_Id          => new String'("Style_Check_Max_Line_Length"),
         Status            => Active,
         Short_Name        => new String'("gnatyM"),
         Description       => null,
         Documentation_Url => null),
      gnatyn =>
        (Human_Id          => new String'("Style_Check_Standard"),
         Status            => Active,
         Short_Name        => new String'("gnatyn"),
         Description       => null,
         Documentation_Url => null),
      gnatyo =>
        (Human_Id          => new String'("Style_Check_Order_Subprograms"),
         Status            => Active,
         Short_Name        => new String'("gnatyo"),
         Description       => null,
         Documentation_Url => null),
      gnatyoo =>
        (Human_Id          => new String'("Style_Check_Missing_Overriding"),
         Status            => Active,
         Short_Name        => new String'("gnatyO"),
         Description       => null,
         Documentation_Url => null),
      gnatyp =>
        (Human_Id          => new String'("Style_Check_Pragma_Casing"),
         Status            => Active,
         Short_Name        => new String'("gnatyp"),
         Description       => null,
         Documentation_Url => null),
      gnatyr =>
        (Human_Id          => new String'("Style_Check_References"),
         Status            => Active,
         Short_Name        => new String'("gnatyr"),
         Description       => null,
         Documentation_Url => null),
      gnatys =>
        (Human_Id          => new String'("Style_Check_Specs"),
         Status            => Active,
         Short_Name        => new String'("gnatys"),
         Description       => null,
         Documentation_Url => null),
      gnatyss =>
        (Human_Id          => new String'("Style_Check_Separate_Stmt_Lines"),
         Status            => Active,
         Short_Name        => new String'("gnatyS"),
         Description       => null,
         Documentation_Url => null),
      gnatytt =>
        (Human_Id          => new String'("Style_Check_Tokens"),
         Status            => Active,
         Short_Name        => new String'("gnatyt"),
         Description       => null,
         Documentation_Url => null),
      gnatyu =>
        (Human_Id          => new String'("Style_Check_Blank_Lines"),
         Status            => Active,
         Short_Name        => new String'("gnatyu"),
         Description       => null,
         Documentation_Url => null),
      gnatyx =>
        (Human_Id          => new String'("Style_Check_Xtra_Parens"),
         Status            => Active,
         Short_Name        => new String'("gnatyx"),
         Description       => null,
         Documentation_Url => null),
      gnatyz =>
        (Human_Id => new String'("Style_Check_Xtra_Parens_Precedence"),
         Status            => Active,
         Short_Name        => new String'("gnatyz"),
         Description       => null,
         Documentation_Url => null),
      gnatel =>
        (Human_Id => new String'("Display_Elaboration_Messages"),
         Status            => Active,
         Short_Name        => new String'("gnatel"),
         Description       => null,
         Documentation_Url => null)
      );

   ----------------
   -- Get_Switch --
   ----------------

   function Get_Switch (Id : Switch_Id) return Switch_Type is

   begin
      return Switches (Id);
   end Get_Switch;

   function Get_Switch (Diag : Diagnostic_Type) return Switch_Type is

   begin
      return Get_Switch (Diag.Switch);
   end Get_Switch;

   -------------------
   -- Get_Switch_Id --
   -------------------

   function Get_Switch_Id (Name : String) return Switch_Id is
      Trimmed_Name : constant String :=
        (if Name (Name'Last) = ' ' then Name (Name'First .. Name'Last - 1)
         else Name);
   begin
      for I in Active_Switch_Id loop
         if Switches (I).Short_Name.all = Trimmed_Name then
            return I;
         end if;
      end loop;

      return No_Switch_Id;
   end Get_Switch_Id;

   -------------------
   -- Get_Switch_Id --
   -------------------

   function Get_Switch_Id (E : Error_Msg_Object) return Switch_Id is
      Switch_Name : constant String :=
        (if E.Warn_Chr = "$ " then "gnatel"
         elsif E.Warn_Chr in "? " | "  " then ""
         elsif E.Kind in Erroutc.Warning | Erroutc.Info
         then "gnatw" & E.Warn_Chr
         elsif E.Kind in Erroutc.Style then "gnatw" & E.Warn_Chr
         else "");
   begin
      if Switch_Name /= "" then
         return Get_Switch_Id (Switch_Name);
      else
         return No_Switch_Id;
      end if;
   end Get_Switch_Id;

   -----------------------------
   -- Print_Switch_Repository --
   -----------------------------

   procedure Print_Switch_Repository is
      First : Boolean := True;
   begin
      Write_Char ('{');
      Begin_Block;
      NL_And_Indent;

      Write_Str ("""" & "Switches" & """" & ": " & "[");
      Begin_Block;

      --  Avoid printing the first switch, which is a placeholder

      for I in Active_Switch_Id loop

         if First then
            First := False;
         else
            Write_Char (',');
         end if;

         NL_And_Indent;

         Write_Char ('{');
         Begin_Block;
         NL_And_Indent;

         if Switches (I).Human_Id /= null then
            Write_String_Attribute ("Human_Id", Switches (I).Human_Id.all);
         else
            Write_String_Attribute ("Human_Id", "null");
         end if;

         Write_Char (',');
         NL_And_Indent;

         if Switches (I).Short_Name /= null then
            Write_String_Attribute ("Short_Name", Switches (I).Short_Name.all);
         else
            Write_String_Attribute ("Short_Name", "null");
         end if;

         Write_Char (',');
         NL_And_Indent;

         if Switches (I).Status = Active then
            Write_String_Attribute ("Status", "Active");
         else
            Write_String_Attribute ("Status", "Deprecated");
         end if;

         Write_Char (',');
         NL_And_Indent;

         if Switches (I).Description /= null then
            Write_String_Attribute ("Description",
                                     Switches (I).Description.all);
         else
            Write_String_Attribute ("Description", "null");
         end if;

         Write_Char (',');
         NL_And_Indent;

         if Switches (I).Description /= null then
            Write_String_Attribute ("Documentation_Url",
                                    Switches (I).Description.all);
         else
            Write_String_Attribute ("Documentation_Url", "null");
         end if;

         End_Block;
         NL_And_Indent;
         Write_Char ('}');
      end loop;

      End_Block;
      NL_And_Indent;
      Write_Char (']');

      End_Block;
      NL_And_Indent;
      Write_Char ('}');

      Write_Eol;
   end Print_Switch_Repository;

end Diagnostics.Switch_Repository;
