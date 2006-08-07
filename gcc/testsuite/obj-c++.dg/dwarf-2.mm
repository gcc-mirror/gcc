/* { dg-options "-gdwarf-2 -dA" } */
/* { dg-skip-if "" { { hppa*-*-hpux* } && { ! hppa*64*-*-* } } { "*" } { "" } } */
/* { dg-final { scan-assembler "0x11\[^0-9a-f\].*DW_AT_language" } } */
int x;
