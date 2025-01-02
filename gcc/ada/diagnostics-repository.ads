------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--               D I A G N O S T I C S . R E P O S I T O R Y                --
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
package Diagnostics.Repository is

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

   Diagnostic_Entries : Diagnostics_Registry_Type :=
     (No_Diagnostic_Id => (others => <>),
      GNAT0001         =>
        (Status        => Active,
         Human_Id      => new String'("Default_Iterator_Not_Primitive_Error"),
         Documentation => new String'("./error_codes/GNAT0001.md"),
         Switch        => No_Switch_Id),
      GNAT0002         =>
        (Status        => Active,
         Human_Id      =>
           new String'("Invalid_Operand_Types_For_Operator_Error"),
         Documentation => new String'("./error_codes/GNAT0002.md"),
         Switch        => No_Switch_Id),
      GNAT0003         =>
        (Status        => Active,
         Human_Id      =>
           new String'("Invalid_Operand_Types_Left_To_Int_Error"),
         Documentation => new String'("./error_codes/GNAT0003.md"),
         Switch        => No_Switch_Id),
      GNAT0004         =>
        (Status        => Active,
         Human_Id      =>
           new String'("Invalid_Operand_Types_Right_To_Int_Error"),
         Documentation => new String'("./error_codes/GNAT0004.md"),
         Switch        => No_Switch_Id),
      GNAT0005         =>
        (Status        => Active,
         Human_Id      =>
           new String'("Invalid_Operand_Types_Left_Acc_Error"),
         Documentation => new String'("./error_codes/GNAT0005.md"),
         Switch        => No_Switch_Id),
      GNAT0006         =>
        (Status        => Active,
         Human_Id      =>
           new String'("Invalid_Operand_Types_Right_Acc_Error"),
         Documentation => new String'("./error_codes/GNAT0006.md"),
         Switch        => No_Switch_Id),
      GNAT0007         =>
        (Status        => Active,
         Human_Id      =>
           new String'("Invalid_Operand_Types_General_Error"),
         Documentation => new String'("./error_codes/GNAT0007.md"),
         Switch        => No_Switch_Id),
      GNAT0008         =>
        (Status        => Active,
         Human_Id      =>
           new String'("Pragma_No_Effect_With_Lock_Free_Warning"),
         Documentation => new String'("./error_codes/GNAT0008.md"),
         Switch        => No_Switch_Id),
      GNAT0009         =>
        (Status        => Active,
         Human_Id      => new String'("End_Loop_Expected_Error"),
         Documentation => new String'("./error_codes/GNAT0009.md"),
         Switch        => No_Switch_Id),
      GNAT0010         =>
        (Status        => Active,
         Human_Id      => new String'("Representation_Too_Late_Error"),
         Documentation => new String'("./error_codes/GNAT0010.md"),
         Switch        => No_Switch_Id),
      GNAT0011         =>
        (Status        => Active,
         Human_Id      => new String'("Mixed_Container_Aggregate_Error"),
         Documentation => new String'("./error_codes/GNAT0011.md"),
         Switch        => No_Switch_Id));

   procedure Print_Diagnostic_Repository;

end Diagnostics.Repository;
