-- with strict dwarf2, we should produce DW_LANG_Ada83 (0x3) as AT_language

-- { dg-do compile }
-- { dg-skip-if "No dwarf-2 support" { hppa*-*-hpux* } }
-- { dg-options "-cargs -gdwarf-2 -gstrict-dwarf -dA -margs" }
-- { dg-final { scan-assembler "0x3\[^\n\r\]*AT_language" } }

procedure Langdw2 is begin null;  end;
   
