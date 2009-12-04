-- { dg-do compile }

with Size_Attribute1_Pkg1;

package Size_Attribute1 is

  function Num return Natural;
  pragma Import (Ada, Num);

  type A is array (Natural range <>) of Integer;

  type T is
    record
      F1 : Long_Float;
      F2 : A (1 .. Num);
    end record;

  package My_Q is new Size_Attribute1_Pkg1 (T);

end Size_Attribute1;
