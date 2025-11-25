------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                E R R I D                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2025, Free Software Foundation, Inc.         --
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

with Erroutc.SARIF_Emitter; use Erroutc.SARIF_Emitter;

package body Errid is

   Doc_Directory : constant String := "./error_codes";
   Doc_Extension : constant String := ".md";

   procedure Add_All_Diagnostic_Rules (Printer : in out SARIF_Printer);
   --  Add all active Diagnostic_Id-s to the SARIF_Printer

   procedure Add_All_Switch_Rules (Printer : in out SARIF_Printer);
   --  Add all active Switch_Id-s to the SARIF_Printer

   ----------------------------
   -- Get_Documentation_File --
   ----------------------------

   function Get_Documentation_File (Id : Diagnostic_Id) return String is
   begin
      if Id = No_Diagnostic_Id then
         return "";
      else
         return Doc_Directory & "/" & To_String (Id) & Doc_Extension;
      end if;
   end Get_Documentation_File;

   ---------------
   -- To_String --
   ---------------

   function To_String (Id : Diagnostic_Id) return String is
   begin
      if Id = No_Diagnostic_Id then
         return "GNAT0000";
      else
         return Id'Img;
      end if;
   end To_String;

   ------------------------------
   -- Add_All_Diagnostic_Rules --
   ------------------------------

   procedure Add_All_Diagnostic_Rules (Printer : in out SARIF_Printer) is
   begin
      Printer.Diagnostics := Diagnostic_Id_Lists.Create;
      for Id in Diagnostic_Id loop
         if Id /= No_Diagnostic_Id then
            Diagnostic_Id_Lists.Append (Printer.Diagnostics, Id);
         end if;
      end loop;
   end Add_All_Diagnostic_Rules;

   --------------------------
   -- Add_All_Switch_Rules --
   --------------------------

   procedure Add_All_Switch_Rules (Printer : in out SARIF_Printer) is
   begin
      Printer.Switches := Switch_Id_Lists.Create;
      for S in Switch_Id loop
         if S /= No_Switch_Id then
            Switch_Id_Lists.Append (Printer.Switches, S);
         end if;
      end loop;
   end Add_All_Switch_Rules;

   ---------------------------------
   -- Print_Diagnostic_Repository --
   ---------------------------------

   procedure Print_Diagnostic_Repository is
      Printer : SARIF_Printer;
   begin
      Add_All_Diagnostic_Rules (Printer);
      Add_All_Switch_Rules (Printer);
      Printer.Report_Type := Repository_Report;

      Print_SARIF_Report (Printer);
      Free (Printer);
   end Print_Diagnostic_Repository;

end Errid;
