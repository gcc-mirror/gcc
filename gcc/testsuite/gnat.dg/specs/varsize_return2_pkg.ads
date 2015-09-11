-- { dg-excess-errors "no code generated" }

package Varsize_Return2_Pkg is

   type T (D: Positive) is record
      Data: String (1 .. D);
   end record;

   function Len return Positive;

   generic
      I : Integer;
   package G is

      subtype Small_T is T(Len);
      function Get return Small_T;

   end G;

end Varsize_Return2_Pkg;
