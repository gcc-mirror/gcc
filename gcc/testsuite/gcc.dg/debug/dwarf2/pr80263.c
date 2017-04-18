/* PR debug/80263 */
/* { dg-do compile } */
/* { dg-options "-g -dA" } */

char array[1];

/* { dg-final { scan-assembler-not {\msizetype} } } */
