/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-skip-if "" { *-*-* }  { "-mror" } { "" } } */

unsigned int rotate (unsigned int a, int b) {
  return ( a >> b ) | ( a << ( 32 - b ) );
}

/* { dg-final { scan-assembler-not "l.ror" } } */
