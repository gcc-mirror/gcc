-- with dwarf3+ or !strict-dwarf, we should produce DW_LANG_Ada95 (0xd)
-- as AT_language

-- { dg-do compile }
-- { dg-skip-if "No dwarf-2 support" { hppa*-*-hpux* } }
-- { dg-options "-cargs -gdwarf-3 -dA -margs" }
-- { dg-final { scan-assembler "0xd\[^\n\r\]*AT_language" } }

procedure Langdw3 is begin null;  end;
   
