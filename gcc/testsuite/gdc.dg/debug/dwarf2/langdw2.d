// DW_LANG_D is not available in dwarf2, so we should produce DW_LANG_C (0x2)
// as AT_language.
// { dg-do compile }
// { dg-options "-gdwarf-2 -gstrict-dwarf -dA" }
// { dg-final { scan-assembler "0x2\[^\n\r\]*AT_language" } }

module langdw2;
