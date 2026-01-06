------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                 E R R O U T C . S A R I F _ E M I T T E R                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2026, Free Software Foundation, Inc.         --
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
with GNAT.Lists; use GNAT.Lists;
package Erroutc.SARIF_Emitter is

   procedure Destroy (Elem : in out Switch_Id) is null;
   pragma Inline (Destroy);
   package Switch_Id_Lists is new Doubly_Linked_Lists
     (Element_Type    => Switch_Id,
      "="             => "=",
      Destroy_Element => Destroy,
      Check_Tampering => False);
   subtype Switch_Id_List is Switch_Id_Lists.Doubly_Linked_List;

   procedure Destroy (Elem : in out Diagnostic_Id) is null;
   pragma Inline (Destroy);
   package Diagnostic_Id_Lists is new Doubly_Linked_Lists
     (Element_Type    => Diagnostic_Id,
      "="             => "=",
      Destroy_Element => Destroy,
      Check_Tampering => False);

   subtype Diagnostic_Id_List is Diagnostic_Id_Lists.Doubly_Linked_List;

   type Report_Kind is (Diagnostic_Report, Repository_Report);
   --  SARIF emitter is used to produce two different types of reports:
   --
   --  Diagnostic_Report - This report includes all of the diagnostics gathered
   --  by the tool under the results, the commandline used to start the tool
   --  under the invocations and all of the unique rules in the emitted
   --  diagnostics under the rules section.
   --
   --  Repository_Report - This report inludes all of the defined
   --  Diagnostic_Id-s and Switch_Id-s as rules but does not contain an
   --  invocation or a result section.

   type SARIF_Printer is record
      Diagnostics : Diagnostic_Id_List;
      --  Unique diagnostics printed as rules in the SARIF report

      Switches : Switch_Id_List;
      --  Unique switches printed as rules in the SARIF report

      Report_Type : Report_Kind := Diagnostic_Report;
      --  The type of report to be printed in SARIF
   end record;

   procedure Free (Self : in out SARIF_Printer);
   --  Free memory for the SARIF_Pritner

   procedure Print_SARIF_Report (Self : SARIF_Printer);
   --  Prints the SARIF report to the currently selected output.

end Erroutc.SARIF_Emitter;
