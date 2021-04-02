// We should produce DW_LANG_D (0x13) as AT_language.
// { dg-do compile }
// { dg-options "-gdwarf-3 -dA" }
// { dg-final { scan-assembler "0x13\[^\n\r\]*AT_language" } }

module langdw3;
