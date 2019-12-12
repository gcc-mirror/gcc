-- { dg-do compile }

with Compile_Time1_Pkg; use Compile_Time1_Pkg;

package Compile_Time1 is

  pragma Compile_Time_Error (Rec'Size /= Integer'Size, "wrong record size");

end Compile_Time1;
