-- { dg-do compile }
-- { dg-skip-if "No Dwarf" { { hppa*-*-hpux* } && { ! lp64 } } }
-- { dg-options "-cargs -O0 -g -dA -fgnat-encodings=minimal -margs" }
--
-- This testcase checks that in the DWARF description of the variant type
-- below, the C discriminant is properly described as unsigned, hence the 0x5a
-- ('Z') and 0x80 (128) values in the DW_AT_discr_list attribute. If it was
-- described as signed, we would have instead 90 and -128.

with Ada.Text_IO;

procedure Debug11 is
   type Rec_Type (C : Character) is record
      case C is
         when 'Z' .. Character'Val (128) => I : Integer;
         when others                     => null;
      end case;
   end record;
   --  R : Rec_Type := ('Z', 2);
   R : Rec_Type ('Z');
begin
   R.I := 0;
   Ada.Text_IO.Put_Line ("" & R.C);
end Debug11;

-- { dg-final { scan-assembler-times "0x5a.*DW_AT_discr_list" 1 } }
-- { dg-final { scan-assembler-times "0x80.*DW_AT_discr_list" 1 } }
