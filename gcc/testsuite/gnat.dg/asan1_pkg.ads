package Asan1_Pkg is
   subtype E is Integer;
   type T is array (1..32) of E;

   function N return T;
   function C (P : T) return E;

   V : constant E := C (N);
end Asan1_Pkg;
