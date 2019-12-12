-- { dg-do compile }
-- { dg-options "-cargs -g -dA -fgnat-encodings=minimal -margs" }

package Debug1 is

   type Index_T is new Positive range 1 .. 128;

   type Array_Type is array (Index_T range <>) of Integer;

   type Record_Type (N : Index_T := 16) is record
      A : Array_Type (1 .. N);
   end record;

   R : Record_Type;

end Debug1;

--  { dg-final { scan-assembler-times "DW_AT_upper_bound" 4 } }
