------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             P R J . A T T R                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 2001-2003 Free Software Foundation, Inc.       --
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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Namet;     use Namet;
with Osint;     use Osint;
with Output;    use Output;

package body Prj.Attr is

   --  Names end with '#'

   --  Package names are preceded by 'P'

   --  Attribute names are preceded by two letters

   --  The first letter is one of
   --    'S' for Single
   --    'L' for list

   --  The second letter is one of
   --    'V' for single variable
   --    'A' for associative array
   --    'a' for case insensitive associative array
   --    'b' for associative array, case insensitive if file names are case
   --        insensitive

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
     "LVmain#" &
     "LVlanguages#" &
     "SVmain_language#" &

   --  package Naming

     "Pnaming#" &
     "Saspecification_suffix#" &
     "Saspec_suffix#" &
     "Saimplementation_suffix#" &
     "Sabody_suffix#" &
     "SVseparate_suffix#" &
     "SVcasing#" &
     "SVdot_replacement#" &
     "SAspecification#" &
     "SAspec#" &
     "SAimplementation#" &
     "SAbody#" &
     "Laspecification_exceptions#" &
     "Laimplementation_exceptions#" &

   --  package Compiler

     "Pcompiler#" &
     "Ladefault_switches#" &
     "Lbswitches#" &
     "SVlocal_configuration_pragmas#" &

   --  package Builder

     "Pbuilder#" &
     "Ladefault_switches#" &
     "Lbswitches#" &
     "SAexecutable#" &
     "SVexecutable_suffix#" &
     "SVglobal_configuration_pragmas#" &

   --  package gnatls

     "Pgnatls#" &
     "LVswitches#" &

   --  package Binder

     "Pbinder#" &
     "Ladefault_switches#" &
     "Lbswitches#" &

   --  package Linker

     "Plinker#" &
     "Ladefault_switches#" &
     "Lbswitches#" &
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

     "#";

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      Start             : Positive           := Initialization_Data'First;
      Finish            : Positive           := Start;
      Current_Package   : Package_Node_Id    := Empty_Package;
      Current_Attribute : Attribute_Node_Id  := Empty_Attribute;
      Is_An_Attribute   : Boolean            := False;
      Kind_1            : Variable_Kind      := Undefined;
      Kind_2            : Attribute_Kind     := Single;
      Package_Name      : Name_Id            := No_Name;
      Attribute_Name    : Name_Id            := No_Name;
      First_Attribute   : Attribute_Node_Id  := Attribute_First;

   begin
      --  Make sure the two tables are empty

      Attributes.Init;
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

               Name_Len := Finish - Start;
               Name_Buffer (1 .. Name_Len) :=
                 To_Lower (Initialization_Data (Start .. Finish - 1));
               Package_Name := Name_Find;

               for Index in Package_First .. Package_Attributes.Last loop
                  if Package_Name = Package_Attributes.Table (Index).Name then
                     Write_Line ("Duplicate package name """ &
                                 Initialization_Data (Start .. Finish - 1) &
                                 """ in Prj.Attr body.");
                     raise Program_Error;
                  end if;
               end loop;

               Is_An_Attribute := False;
               Current_Attribute := Empty_Attribute;
               Package_Attributes.Increment_Last;
               Current_Package := Package_Attributes.Last;
               Package_Attributes.Table (Current_Package).Name :=
                 Package_Name;
               Start := Finish + 1;

            when 'S' =>
               Kind_1 := Single;

            when 'L' =>
               Kind_1 := List;

            when others =>
               raise Program_Error;
         end case;

         if Is_An_Attribute then

            --  New attribute

            Start := Start + 1;
            case Initialization_Data (Start) is
               when 'V' =>
                  Kind_2 := Single;

               when 'A' =>
                  Kind_2 := Associative_Array;

               when 'a' =>
                  Kind_2 := Case_Insensitive_Associative_Array;

               when 'b' =>
                  if File_Names_Case_Sensitive then
                     Kind_2 := Case_Insensitive_Associative_Array;
                  else
                     Kind_2 := Case_Insensitive_Associative_Array;
                  end if;

               when others =>
                  raise Program_Error;
            end case;

            Start := Start + 1;
            Finish := Start;

            while Initialization_Data (Finish) /= '#' loop
               Finish := Finish + 1;
            end loop;

            Name_Len := Finish - Start;
            Name_Buffer (1 .. Name_Len) :=
              To_Lower (Initialization_Data (Start .. Finish - 1));
            Attribute_Name := Name_Find;
            Attributes.Increment_Last;
            if Current_Attribute = Empty_Attribute then
               First_Attribute := Attributes.Last;

               if Current_Package /= Empty_Package then
                  Package_Attributes.Table (Current_Package).First_Attribute
                    := Attributes.Last;
               end if;

            else
               --  Check that there are no duplicate attributes

               for Index in First_Attribute .. Attributes.Last - 1 loop
                  if Attribute_Name =
                    Attributes.Table (Index).Name then
                     Write_Line ("Duplicate attribute name """ &
                                 Initialization_Data (Start .. Finish - 1) &
                                 """ in Prj.Attr body.");
                     raise Program_Error;
                  end if;
               end loop;

               Attributes.Table (Current_Attribute).Next :=
                 Attributes.Last;
            end if;

            Current_Attribute := Attributes.Last;
            Attributes.Table (Current_Attribute) :=
              (Name    => Attribute_Name,
               Kind_1  => Kind_1,
               Kind_2  => Kind_2,
               Next    => Empty_Attribute);
            Start := Finish + 1;
         end if;
      end loop;
   end Initialize;

end Prj.Attr;
