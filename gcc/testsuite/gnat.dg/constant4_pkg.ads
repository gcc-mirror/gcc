with Ada.Command_Line;       use Ada.Command_Line;
with System.Multiprocessors; use System.Multiprocessors;

package Constant4_Pkg is

   Max_CPUs : constant CPU := (if Argument_Count < 2 then Number_Of_CPUs
                               else CPU'Value (Argument (2)));

   subtype Worker_Id is CPU range 1 .. Max_CPUs;

   type Counter is range 0 .. 10**18;

   Steals : array (Worker_Id) of Counter := (others => 0);

end Constant4_Pkg;
