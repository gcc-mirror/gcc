// { dg-do compile }
// { dg-options "-O -g -dA -gno-strict-dwarf" }
// { dg-additional-options "-fno-common" { target hppa*-*-hpux* } }
// { dg-final { scan-assembler-times " DW_AT_alignment" 1 } }

int _Alignas(64) i;
