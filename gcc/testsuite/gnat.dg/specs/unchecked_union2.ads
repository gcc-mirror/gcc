-- { dg-do compile }

package Unchecked_Union2 is

   type Small_Int is range 0 .. 2**19 - 1;

   type R1 (B : Boolean := True) is record
      case B is
         when True  => Data1 : Small_Int;
         when False => Data2 : Small_Int;
      end case;
   end record;

   for R1 use record
      Data1 at 0 range 0 .. 18;
      Data2 at 0 range 0 .. 18;
   end record;
   for R1'Size use 24;

   pragma Unchecked_Union (R1);

   type R2 is record
     Data : R1;
   end record;

   for R2 use record
     Data at 0 range 3 .. 26;
   end record;

end Unchecked_Union2;
