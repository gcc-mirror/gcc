/* { dg-do compile } */
/* { dg-options "-O -std=c89 -g -dA" } */
/* DW_LANG_C89 = 0x0001 */
/* { dg-final { scan-assembler "0x1.*DW_AT_language" { xfail { powerpc-ibm-aix* } } } } */

int version;
