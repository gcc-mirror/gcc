/* { dg-do compile } */
/* { dg-options "-mror -O2" } */

unsigned int rotate6 (unsigned int a) {
  return ( a >> 6 ) | ( a << ( 32 - 6 ) );
}

/* { dg-final { scan-assembler-not "l.rori" } } */
