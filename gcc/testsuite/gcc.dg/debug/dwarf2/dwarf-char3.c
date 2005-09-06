/* PR debug/7241 */
/* { dg-do compile } */
/* { dg-options "-O2 -gdwarf-2 -dA" } */
/* { dg-final { scan-assembler "0x\[68\]\[ \t\]\[#@;!/|\]+ +DW_AT_encoding" } } */
/* { dg-final { scan-assembler-not "0x\[57\]\[ \t\]\[#@;!/|\]+ +DW_AT_encoding" } } */

const char *p = "abc";
