/* { dg-options "-gdwarf-2 -dA" } */
/* { dg-final { scan-assembler "0x10\[^0-9a-f\].*DW_AT_language" } } */
/* { dg-skip-if "No Dwarf" { { *-*-aix* alpha*-dec-osf* hppa*-*-hpux* } && { ! hppa*64*-*-* } } { "*" } { "" } } */
int x;
