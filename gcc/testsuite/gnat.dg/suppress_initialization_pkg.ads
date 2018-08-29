with Interfaces; use Interfaces;
with System;

package Suppress_Initialization_Pkg is

   type Discriminated_Type (Foo : Unsigned_8 := 0) is record
      case Foo is
         when 0 =>
            Bar  : Boolean;
         when 1 =>
            Baz  : Unsigned_32;
         when others =>
            null;
      end case;
   end record;

   for Discriminated_Type use record
      Foo at 0 range  0 ..  7;
      Bar at 1 range  0 ..  0;
      Baz at 1 range  0 .. 31;
   end record;

   External : Discriminated_Type
   with
     Volatile,
     Suppress_Initialization,
     Address => System'To_Address (16#1234_5678#);

   procedure Read;

end Suppress_Initialization_Pkg;
