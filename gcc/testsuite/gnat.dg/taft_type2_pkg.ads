package Taft_Type2_Pkg  is

   type T is private;

   function Open return T;

private

   type Buffer_T;
   type T is access Buffer_T;

end Taft_Type2_Pkg;
