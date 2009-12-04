-- { dg-excess-errors "no code generated" }

with Size_Attribute1_Pkg2;

generic

  type T is private;

package Size_Attribute1_Pkg1 is

  package My_R is new Size_Attribute1_Pkg2 (T);

  procedure Dummy;

end Size_Attribute1_Pkg1;
