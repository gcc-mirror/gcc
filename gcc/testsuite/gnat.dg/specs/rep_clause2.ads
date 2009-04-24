-- { dg-do compile }
-- { dg-options "-gnatws" }

package Rep_Clause2 is

  type S is new String;
  for S'Component_Size use 256;

  type T is new S(1..8);

end Rep_Clause2;
