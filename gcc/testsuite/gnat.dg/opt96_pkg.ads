with System;

package Opt96_Pkg is

   type Baz_Type is delta (1.0 / 2.0**16) range 0.0 .. 1.0 - (1.0 / 2.0**16);
   for Baz_Type'Small use (1.0 / 2.0**16);
   for Baz_Type'Size use 16;

   type Bar_Type is record
     X : Baz_Type;
     Y : Baz_Type;
   end record;
   for Bar_Type use record
     X at 0 range 0 .. 15;
     Y at 2 range 0 .. 15;
   end record;
   for Bar_Type'Bit_Order use System.High_Order_First;
   for Bar_Type'Scalar_Storage_Order use System.High_Order_First;

   type Foo_Type is record
      Bar : Bar_Type;
   end record;

   type Data is tagged record
     Foo : Foo_Type;
   end record;

   type Rec is tagged null record;

   function F (Self : Rec; D  : Data'Class) return Integer;

end Opt96_Pkg;
