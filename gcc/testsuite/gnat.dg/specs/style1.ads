-- { dg-do compile }
-- { dg-options "-gnatyr" }

with Ada.Containers.Vectors;
with Ada.Unchecked_Conversion;

package Style1 is

  package My_Vector is new ada.containers.vectors -- { dg-warning " bad casing" }
    (Index_Type   => Positive,
     Element_Type => Integer);

  type Word is mod 2**32;

  function My_Conv is new ada.unchecked_conversion -- { dg-warning " bad casing" }
    (Source => Integer,
     Target => Word);

end Style1;
