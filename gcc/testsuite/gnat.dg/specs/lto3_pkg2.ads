-- { dg-excess-errors "no code generated" }

generic
  type Id_T is private;
  type Data_T is private;
package Lto3_Pkg2 is
  type T is private;
  function Get (X : T) return Data_T;
private
  type T is null record;
end Lto3_Pkg2;
