/* { dg-do compile } */
/* { dg-options "-mror -O2" } */

unsigned int rotate (unsigned int a, int b) {
  return ( a >> b ) | ( a << ( 32 - b ) );
}

/* { dg-final { scan-assembler "l.ror" } } */
