generic

  type T is private;

package Compile_Time_Error1_Pkg is

  pragma Compile_Time_Error (T'Size not in 8 | 16 | 32, "type too large");

  Data : T;

end Compile_Time_Error1_Pkg;
