generic

   type T is private;
   Init_Value : T;

package Opt25_Pkg1 is

   Var : T := Init_Value;
   procedure Swap (A, B : in out T);

end Opt25_Pkg1;
