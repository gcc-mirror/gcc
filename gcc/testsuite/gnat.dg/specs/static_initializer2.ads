-- { dg-do compile }

package Static_Initializer2 is

   type A is array (1..5) of Integer;
   f : constant A := (1, 2, 3, 4, 5);

   i1 : integer renames f(1);
   i2 : integer renames f(2);
   i3 : integer renames f(3);
   i4 : integer renames f(4);
   i5 : integer renames f(5);

   b1 : boolean := i1 = 1;
   b2 : boolean := i2 = 2;
   b3 : boolean := i3 = 3;
   b4 : boolean := i4 = 4;
   b5 : boolean := i5 = 5;

end Static_Initializer2;

-- { dg-final { scan-assembler-not "elabs" } }
