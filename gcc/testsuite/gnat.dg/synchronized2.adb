with Synchronized2_Pkg;
package body Synchronized2 with SPARK_Mode, Refined_State => (State => C) is
   C : Synchronized2_Pkg.T;
   procedure Dummy is null;
end;
