------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             P R J . A T T R                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 2001-2004 Free Software Foundation, Inc.       --
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

with Namet;   use Namet;
with Osint;
with Prj.Com; use Prj.Com;
with Table;

with System.Case_Util; use System.Case_Util;

package body Prj.Attr is

   --  Data for predefined attributes and packages

   --  Names end with '#'

   --  Package names are preceded by 'P'

   --  Attribute names are preceded by two letters:

   --  The first letter is one of
   --    'S' for Single
   --    's' for Single with optional index
   --    'L' for List
   --    'l' for List of strings with optional indexes

   --  The second letter is one of
   --    'V' for single variable
   --    'A' for associative array
   --    'a' for case insensitive associative array
   --    'b' for associative array, case insensitive if file names are case
   --        insensitive
   --    'c' same as 'b', with optional index

   --  End is indicated by two consecutive '#'.

   Initialization_Data : constant String :=

   --  project attributes

     "SVobject_dir#" &
     "SVexec_dir#" &
     "LVsource_dirs#" &
     "LVsource_files#" &
     "LVlocally_removed_files#" &
     "SVsource_list_file#" &
     "SVlibrary_dir#" &
     "SVlibrary_name#" &
     "SVlibrary_kind#" &
     "SVlibrary_version#" &
     "LVlibrary_interface#" &
     "SVlibrary_auto_init#" &
     "LVlibrary_options#" &
     "SVlibrary_src_dir#" &
     "SVlibrary_gcc#" &
     "SVlibrary_symbol_file#" &
     "SVlibrary_symbol_policy#" &
     "SVlibrary_reference_symbol_file#" &
     "lVmain#" &
     "LVlanguages#" &
     "SVmain_language#" &
     "LVada_roots#" &
     "SVexternally_built#" &

   --  package Naming

     "Pnaming#" &
     "Saspecification_suffix#" &
     "Saspec_suffix#" &
     "Saimplementation_suffix#" &
     "Sabody_suffix#" &
     "SVseparate_suffix#" &
     "SVcasing#" &
     "SVdot_replacement#" &
     "sAspecification#" &
     "sAspec#" &
     "sAimplementation#" &
     "sAbody#" &
     "Laspecification_exceptions#" &
     "Laimplementation_exceptions#" &

   --  package Compiler

     "Pcompiler#" &
     "Ladefault_switches#" &
     "Lcswitches#" &
     "SVlocal_configuration_pragmas#" &

   --  package Builder

     "Pbuilder#" &
     "Ladefault_switches#" &
     "Lcswitches#" &
     "Scexecutable#" &
     "SVexecutable_suffix#" &
     "SVglobal_configuration_pragmas#" &

   --  package gnatls

     "Pgnatls#" &
     "LVswitches#" &

   --  package Binder

     "Pbinder#" &
     "Ladefault_switches#" &
     "Lcswitches#" &

   --  package Linker

     "Plinker#" &
     "Ladefault_switches#" &
     "Lcswitches#" &
     "LVlinker_options#" &

   --  package Cross_Reference

     "Pcross_reference#" &
     "Ladefault_switches#" &
     "Lbswitches#" &

   --  package Finder

     "Pfinder#" &
     "Ladefault_switches#" &
     "Lbswitches#" &

   --  package Pretty_Printer

     "Ppretty_printer#" &
     "Ladefault_switches#" &
     "Lbswitches#" &

   --  package gnatstub

     "Pgnatstub#" &
     "Ladefault_switches#" &
     "Lbswitches#" &

   --  package Eliminate

     "Peliminate#" &
     "Ladefault_switches#" &
     "Lbswitches#" &

   --  package Metrics

     "Pmetrics#" &
     "Ladefault_switches#" &
     "Lbswitches#" &

   --  package Ide

     "Pide#" &
     "Ladefault_switches#" &
     "SVremote_host#" &
     "SVprogram_host#" &
     "SVcommunication_protocol#" &
     "Sacompiler_command#" &
     "SVdebugger_command#" &
     "SVgnatlist#" &
     "SVvcs_kind#" &
     "SVvcs_file_check#" &
     "SVvcs_log_check#" &

   --  package Language_Processing

     "Planguage_processing#" &
     "Lacompiler_driver#" &
     "Sacompiler_kind#" &
     "Ladependency_option#" &
     "Lacompute_dependency#" &
     "Lainclude_option#" &
     "Sabinder_driver#" &
     "SVdefault_linker#" &

     "#";

   Initialized : Boolean := False;
   --  A flag to avoid multiple initialization

   function Name_Id_Of (Name : String) return Name_Id;
   --  Returns the Name_Id for Name in lower case

   -----------------------
   -- Attribute_Kind_Of --
   -----------------------

   function Attribute_Kind_Of
     (Attribute : Attribute_Node_Id) return Attribute_Kind
   is
   begin
      if Attribute = Empty_Attribute then
         return Unknown;
      else
         return Attrs.Table (Attribute.Value).Attr_Kind;
      end if;
   end Attribute_Kind_Of;

   -----------------------
   -- Attribute_Name_Of --
   -----------------------

   function Attribute_Name_Of (Attribute : Attribute_Node_Id) return Name_Id is
   begin
      if Attribute = Empty_Attribute then
         return No_Name;
      else
         return Attrs.Table (Attribute.Value).Name;
      end if;
   end Attribute_Name_Of;

   --------------------------
   -- Attribute_Node_Id_Of --
   --------------------------

   function Attribute_Node_Id_Of
     (Name        : Name_Id;
      Starting_At : Attribute_Node_Id) return Attribute_Node_Id
   is
      Id : Attr_Node_Id := Starting_At.Value;

   begin
      while Id /= Empty_Attr
        and then Attrs.Table (Id).Name /= Name
      loop
         Id := Attrs.Table (Id).Next;
      end loop;

      return (Value => Id);
   end Attribute_Node_Id_Of;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      Start             : Positive          := Initialization_Data'First;
      Finish            : Positive          := Start;
      Current_Package   : Pkg_Node_Id       := Empty_Pkg;
      Current_Attribute : Attr_Node_Id      := Empty_Attr;
      Is_An_Attribute   : Boolean           := False;
      Var_Kind          : Variable_Kind     := Undefined;
      Optional_Index    : Boolean           := False;
      Attr_Kind            : Attribute_Kind := Single;
      Package_Name      : Name_Id           := No_Name;
      Attribute_Name    : Name_Id           := No_Name;
      First_Attribute   : Attr_Node_Id      := Attr.First_Attribute;

      function Attribute_Location return String;
      --  Returns a string depending if we are in the project level attributes
      --  or in the attributes of a package.

      ------------------------
      -- Attribute_Location --
      ------------------------

      function Attribute_Location return String is
      begin
         if Package_Name = No_Name then
            return "project level attributes";

         else
            return "attribute of package """ &
            Get_Name_String (Package_Name) & """";
         end if;
      end Attribute_Location;

   --  Start of processing for Initialize

   begin
      --  Don't allow Initialize action to be repeated

      if Initialized then
         return;
      end if;

      --  Make sure the two tables are empty

      Attrs.Init;
      Package_Attributes.Init;

      while Initialization_Data (Start) /= '#' loop
         Is_An_Attribute := True;
         case Initialization_Data (Start) is
            when 'P' =>

               --  New allowed package

               Start := Start + 1;

               Finish := Start;
               while Initialization_Data (Finish) /= '#' loop
                  Finish := Finish + 1;
               end loop;

               Package_Name :=
                 Name_Id_Of (Initialization_Data (Start .. Finish - 1));

               for Index in First_Package .. Package_Attributes.Last loop
                  if Package_Name = Package_Attributes.Table (Index).Name then
                     Osint.Fail ("duplicate name """,
                           Initialization_Data (Start .. Finish - 1),
                           """ in predefined packages.");
                  end if;
               end loop;

               Is_An_Attribute := False;
               Current_Attribute := Empty_Attr;
               Package_Attributes.Increment_Last;
               Current_Package := Package_Attributes.Last;
               Package_Attributes.Table (Current_Package) :=
                 (Name            => Package_Name,
                  Known           => True,
                  First_Attribute => Empty_Attr);
               Start := Finish + 1;

            when 'S' =>
               Var_Kind       := Single;
               Optional_Index := False;

            when 's' =>
               Var_Kind       := Single;
               Optional_Index := True;

            when 'L' =>
               Var_Kind       := List;
               Optional_Index := False;

            when 'l' =>
               Var_Kind         := List;
               Optional_Index := True;

            when others =>
               raise Program_Error;
         end case;

         if Is_An_Attribute then

            --  New attribute

            Start := Start + 1;
            case Initialization_Data (Start) is
               when 'V' =>
                  Attr_Kind := Single;

               when 'A' =>
                  Attr_Kind := Associative_Array;

               when 'a' =>
                  Attr_Kind := Case_Insensitive_Associative_Array;

               when 'b' =>
                  if Osint.File_Names_Case_Sensitive then
                     Attr_Kind := Associative_Array;
                  else
                     Attr_Kind := Case_Insensitive_Associative_Array;
                  end if;

               when 'c' =>
                  if Osint.File_Names_Case_Sensitive then
                     Attr_Kind := Optional_Index_Associative_Array;
                  else
                     Attr_Kind :=
                       Optional_Index_Case_Insensitive_Associative_Array;
                  end if;

               when others =>
                  raise Program_Error;
            end case;

            Start := Start + 1;
            Finish := Start;

            while Initialization_Data (Finish) /= '#' loop
               Finish := Finish + 1;
            end loop;

            Attribute_Name :=
              Name_Id_Of (Initialization_Data (Start .. Finish - 1));
            Attrs.Increment_Last;

            if Current_Attribute = Empty_Attr then
               First_Attribute := Attrs.Last;

               if Current_Package /= Empty_Pkg then
                  Package_Attributes.Table (Current_Package).First_Attribute
                    := Attrs.Last;
               end if;

            else
               --  Check that there are no duplicate attributes

               for Index in First_Attribute .. Attrs.Last - 1 loop
                  if Attribute_Name = Attrs.Table (Index).Name then
                     Osint.Fail ("duplicate attribute """,
                           Initialization_Data (Start .. Finish - 1),
                           """ in " & Attribute_Location);
                  end if;
               end loop;

               Attrs.Table (Current_Attribute).Next :=
                 Attrs.Last;
            end if;

            Current_Attribute := Attrs.Last;
            Attrs.Table (Current_Attribute) :=
              (Name           => Attribute_Name,
               Var_Kind       => Var_Kind,
               Optional_Index => Optional_Index,
               Attr_Kind      => Attr_Kind,
               Next           => Empty_Attr);
            Start := Finish + 1;
         end if;
      end loop;

      Initialized := True;
   end Initialize;

   ----------------
   -- Name_Id_Of --
   ----------------

   function Name_Id_Of (Name : String) return Name_Id is
   begin
      Name_Len := 0;
      Add_Str_To_Name_Buffer (Name);
      To_Lower (Name_Buffer (1 .. Name_Len));
      return Name_Find;
   end Name_Id_Of;

   --------------------
   -- Next_Attribute --
   --------------------

   function Next_Attribute
     (After : Attribute_Node_Id) return Attribute_Node_Id
   is
   begin
      if After = Empty_Attribute then
         return Empty_Attribute;
      else
         return (Value => Attrs.Table (After.Value).Next);
      end if;
   end Next_Attribute;

   -----------------------
   -- Optional_Index_Of --
   -----------------------

   function Optional_Index_Of (Attribute : Attribute_Node_Id) return Boolean is
   begin
      if Attribute = Empty_Attribute then
         return False;
      else
         return Attrs.Table (Attribute.Value).Optional_Index;
      end if;
   end Optional_Index_Of;

   ------------------------
   -- Package_Node_Id_Of --
   ------------------------

   function Package_Node_Id_Of (Name : Name_Id) return Package_Node_Id is
   begin
      for Index in Package_Attributes.First .. Package_Attributes.Last loop
         if Package_Attributes.Table (Index).Name = Name then
            return (Value => Index);
         end if;
      end loop;

      --  If there is no package with this name, return Empty_Package

      return Empty_Package;
   end Package_Node_Id_Of;

   ----------------------------
   -- Register_New_Attribute --
   ----------------------------

   procedure Register_New_Attribute
     (Name               : String;
      In_Package         : Package_Node_Id;
      Attr_Kind          : Defined_Attribute_Kind;
      Var_Kind           : Defined_Variable_Kind;
      Index_Is_File_Name : Boolean := False;
      Opt_Index          : Boolean := False)
   is
      Attr_Name       : Name_Id;
      First_Attr      : Attr_Node_Id := Empty_Attr;
      Curr_Attr       : Attr_Node_Id;
      Real_Attr_Kind  : Attribute_Kind;

   begin
      if Name'Length = 0 then
         Fail ("cannot register an attribute with no name");
         raise Project_Error;
      end if;

      if In_Package = Empty_Package then
         Fail ("attempt to add attribute """, Name,
               """ to an undefined package");
         raise Project_Error;
      end if;

      Attr_Name := Name_Id_Of (Name);

      First_Attr :=
        Package_Attributes.Table (In_Package.Value).First_Attribute;

      --  Check if attribute name is a duplicate

      Curr_Attr := First_Attr;
      while Curr_Attr /= Empty_Attr loop
         if Attrs.Table (Curr_Attr).Name = Attr_Name then
            Fail ("duplicate attribute name """, Name,
                  """ in package """ &
                  Get_Name_String
                    (Package_Attributes.Table (In_Package.Value).Name) &
                  """");
            raise Project_Error;
         end if;

         Curr_Attr := Attrs.Table (Curr_Attr).Next;
      end loop;

      Real_Attr_Kind := Attr_Kind;

      --  If Index_Is_File_Name, change the attribute kind if necessary

      if Index_Is_File_Name and then not Osint.File_Names_Case_Sensitive then
         case Attr_Kind is
            when Associative_Array =>
               Real_Attr_Kind := Case_Insensitive_Associative_Array;

            when Optional_Index_Associative_Array =>
               Real_Attr_Kind :=
                 Optional_Index_Case_Insensitive_Associative_Array;

            when others =>
               null;
         end case;
      end if;

      --  Add the new attribute

      Attrs.Increment_Last;
      Attrs.Table (Attrs.Last) :=
        (Name           => Attr_Name,
         Var_Kind       => Var_Kind,
         Optional_Index => Opt_Index,
         Attr_Kind      => Real_Attr_Kind,
         Next           => First_Attr);
      Package_Attributes.Table (In_Package.Value).First_Attribute :=
        Attrs.Last;
   end Register_New_Attribute;

   --------------------------
   -- Register_New_Package --
   --------------------------

   procedure Register_New_Package (Name : String; Id : out Package_Node_Id) is
      Pkg_Name : Name_Id;

   begin
      if Name'Length = 0 then
         Fail ("cannot register a package with no name");
         Id := Empty_Package;
         return;
      end if;

      Pkg_Name := Name_Id_Of (Name);

      for Index in Package_Attributes.First .. Package_Attributes.Last loop
         if Package_Attributes.Table (Index).Name = Pkg_Name then
            Fail ("cannot register a package with a non unique name""",
                  Name, """");
            Id := Empty_Package;
            return;
         end if;
      end loop;

      Package_Attributes.Increment_Last;
      Id := (Value => Package_Attributes.Last);
      Package_Attributes.Table (Package_Attributes.Last) :=
        (Name => Pkg_Name, Known => True, First_Attribute => Empty_Attr);
   end Register_New_Package;

   procedure Register_New_Package
     (Name       : String;
      Attributes : Attribute_Data_Array)
   is
      Pkg_Name   : Name_Id;
      Attr_Name  : Name_Id;
      First_Attr : Attr_Node_Id := Empty_Attr;
      Curr_Attr  : Attr_Node_Id;
      Attr_Kind  : Attribute_Kind;

   begin
      if Name'Length = 0 then
         Fail ("cannot register a package with no name");
         raise Project_Error;
      end if;

      Pkg_Name := Name_Id_Of (Name);

      for Index in Package_Attributes.First .. Package_Attributes.Last loop
         if Package_Attributes.Table (Index).Name = Pkg_Name then
            Fail ("cannot register a package with a non unique name""",
                  Name, """");
            raise Project_Error;
         end if;
      end loop;

      for Index in Attributes'Range loop
         Attr_Name := Name_Id_Of (Attributes (Index).Name);

         Curr_Attr := First_Attr;
         while Curr_Attr /= Empty_Attr loop
            if Attrs.Table (Curr_Attr).Name = Attr_Name then
               Fail ("duplicate attribute name """, Attributes (Index).Name,
                     """ in new package """ & Name & """");
               raise Project_Error;
            end if;

            Curr_Attr := Attrs.Table (Curr_Attr).Next;
         end loop;

         Attr_Kind := Attributes (Index).Attr_Kind;

         if Attributes (Index).Index_Is_File_Name
           and then not Osint.File_Names_Case_Sensitive
         then
            case Attr_Kind is
               when Associative_Array =>
                  Attr_Kind := Case_Insensitive_Associative_Array;

               when Optional_Index_Associative_Array =>
                  Attr_Kind :=
                    Optional_Index_Case_Insensitive_Associative_Array;

               when others =>
                  null;
            end case;
         end if;

         Attrs.Increment_Last;
         Attrs.Table (Attrs.Last) :=
           (Name           => Attr_Name,
            Var_Kind       => Attributes (Index).Var_Kind,
            Optional_Index => Attributes (Index).Opt_Index,
            Attr_Kind      => Attr_Kind,
            Next           => First_Attr);
         First_Attr := Attrs.Last;
      end loop;

      Package_Attributes.Increment_Last;
      Package_Attributes.Table (Package_Attributes.Last) :=
        (Name => Pkg_Name, Known => True, First_Attribute => First_Attr);
   end Register_New_Package;

   ---------------------------
   -- Set_Attribute_Kind_Of --
   ---------------------------

   procedure Set_Attribute_Kind_Of
     (Attribute : Attribute_Node_Id;
      To        : Attribute_Kind)
   is
   begin
      if Attribute /= Empty_Attribute then
         Attrs.Table (Attribute.Value).Attr_Kind := To;
      end if;
   end Set_Attribute_Kind_Of;

   --------------------------
   -- Set_Variable_Kind_Of --
   --------------------------

   procedure Set_Variable_Kind_Of
     (Attribute : Attribute_Node_Id;
      To        : Variable_Kind)
   is
   begin
      if Attribute /= Empty_Attribute then
         Attrs.Table (Attribute.Value).Var_Kind := To;
      end if;
   end Set_Variable_Kind_Of;

   ----------------------
   -- Variable_Kind_Of --
   ----------------------

   function Variable_Kind_Of
     (Attribute : Attribute_Node_Id) return Variable_Kind
   is
   begin
      if Attribute = Empty_Attribute then
         return Undefined;
      else
         return Attrs.Table (Attribute.Value).Var_Kind;
      end if;
   end Variable_Kind_Of;

   ------------------------
   -- First_Attribute_Of --
   ------------------------

   function First_Attribute_Of
     (Pkg : Package_Node_Id) return Attribute_Node_Id
   is
   begin
      if Pkg = Empty_Package then
         return Empty_Attribute;
      else
         return
           (Value => Package_Attributes.Table (Pkg.Value).First_Attribute);
      end if;
   end First_Attribute_Of;

end Prj.Attr;
