------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                E R R I D                                 --
--                                                                          --
--                                 S p e c                                  --
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

with Types; use Types;
with Errsw; use Errsw;

package Errid is

   type Status_Type is
     (Active,
      Deprecated);

   type Diagnostic_Id is
     (No_Diagnostic_Id,
      GNAT0001,
      GNAT0002,
      GNAT0003,
      GNAT0004,
      GNAT0005,
      GNAT0006);

   function To_String (Id : Diagnostic_Id) return String;
   --  Convert the diagnostic ID to a 4 character string padded with 0-s.

   type Diagnostic_Entry_Type is record
      Status : Status_Type := Active;

      Human_Id : String_Ptr := null;
      --  A human readable code for the diagnostic. If the diagnostic has a
      --  switch with a human id then the human_id of the switch shall be used
      --  in SARIF reports.

      Documentation : String_Ptr := null;

      Switch : Switch_Id := No_Switch_Id;
      --  The switch that controls the diagnostic message.
   end record;

   type Diagnostics_Registry_Type is
     array (Diagnostic_Id) of Diagnostic_Entry_Type;

   --  Include the diagnostic entries for every diagnostic id.
   --  The entries should include:
   --  * Whether the diagnostic with this id is active or not
   --  * The human-readable name for the diagnostic for SARIF reports
   --  * The switch id for the diagnostic if the diagnostic is linked to any
   --    compiler switch
   --  * The documentation file for the diagnostic written in the MD format.
   --    The documentation file should include:
   --    - The diagnostic id
   --    - A short description of the diagnostic
   --    - A minimal example of the code that triggers the diagnostic
   --    - An explanation of why the diagnostic was triggered
   --    - A suggestion on how to fix the issue
   --    - Optionally additional information
   --    TODO: the mandatory fields for the documentation file could be changed

   Diagnostic_Entries : constant Diagnostics_Registry_Type :=
     (No_Diagnostic_Id => <>,
      GNAT0001         =>
        (Status        => Active,
         Human_Id      => new String'("Default_Iterator_Not_Primitive_Error"),
         Documentation => new String'("./error_codes/GNAT0001.md"),
         Switch        => No_Switch_Id),
      GNAT0002         =>
        (Status        => Active,
         Human_Id      =>
           new String'("Invalid_Operand_Types_General_Error"),
         Documentation => new String'("./error_codes/GNAT0007.md"),
         Switch        => No_Switch_Id),
      GNAT0003         =>
        (Status        => Active,
         Human_Id      =>
           new String'("Pragma_No_Effect_With_Lock_Free_Warning"),
         Documentation => new String'("./error_codes/GNAT0008.md"),
         Switch        => No_Switch_Id),
      GNAT0004         =>
        (Status        => Active,
         Human_Id      => new String'("End_Loop_Expected_Error"),
         Documentation => new String'("./error_codes/GNAT0009.md"),
         Switch        => No_Switch_Id),
      GNAT0005         =>
        (Status        => Active,
         Human_Id      => new String'("Representation_Too_Late_Error"),
         Documentation => new String'("./error_codes/GNAT0010.md"),
         Switch        => No_Switch_Id),
      GNAT0006         =>
        (Status        => Active,
         Human_Id      => new String'("Mixed_Container_Aggregate_Error"),
         Documentation => new String'("./error_codes/GNAT0011.md"),
         Switch        => No_Switch_Id));

   procedure Print_Diagnostic_Repository;

end Errid;
