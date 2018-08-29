/* { dg-options "-gdwarf-2 -dA -gno-strict-dwarf" } */
/* { dg-final { scan-assembler "0x10\[^0-9a-f\].*DW_AT_language" } } */
/* { dg-skip-if "No Dwarf" { { *-*-aix* hppa*-*-hpux* } && { ! hppa*64*-*-* } } } */
int x;
