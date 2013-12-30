/* PR target/59501 */
/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mavx -maccumulate-outgoing-args" } */

#include "pr59501-3a.c"

/* Verify no dynamic realignment is performed.  */
/* { dg-final { scan-assembler-not "and\[^\n\r]*sp" { xfail *-*-* } } } */
