with Array23_Pkg3;

package Array23_Pkg2 is

   C1 : Natural := Array23_Pkg3.C0;

   type Enum is (Zero, One, Two);

   subtype Index is Enum range One .. Enum'val(C1);

end Array23_Pkg2;
