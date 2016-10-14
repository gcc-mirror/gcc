-- { dg-do compile }
-- { dg-options "-cargs -g -fgnat-encodings=minimal -dA -margs" }
-- { dg-final { scan-assembler-not "DW_OP_const4u" } }
-- { dg-final { scan-assembler-not "DW_OP_const8u" } }

--  The DW_AT_byte_size attribute DWARF expression for the
--  DW_TAG_structure_type DIE that describes Rec_Type contains the -4u literal.
--  Check that it is not created using an inefficient encoding (DW_OP_const1s
--  is expected).

procedure Debug8 is

   type Rec_Type (I : Integer) is record
      B : Boolean;
      case I is
         when 0 =>
            null;
         when 1 .. 10 =>
            C : Character;
         when others =>
            N : Natural;
      end case;
   end record;

   R : access Rec_Type := null;

begin
   null;
end Debug8;
