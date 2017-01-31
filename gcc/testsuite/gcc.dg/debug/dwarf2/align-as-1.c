// { dg-do compile }
// { dg-options "-O -g -dA" }
// { dg-final { scan-assembler-times " DW_AT_alignment" 1 { xfail { powerpc-ibm-aix* } } } }

int _Alignas(64) i;
