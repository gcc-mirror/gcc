-- { dg-compile }

package Small_Alignment is

  type Int is range -512 .. 511;
  for Int'Alignment use 1;

  type R is record
    I: Int;
  end record;
  Pragma Pack (R);

end Small_Alignment;
