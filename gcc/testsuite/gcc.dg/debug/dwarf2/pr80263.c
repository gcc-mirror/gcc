/* PR debug/80263 */
/* { dg-do compile } */
/* { dg-options "-g -dA" } */
/* Darwin emits pubnames/types by default - suppress this for the test. */
/* { dg-additional-options "-gno-pubnames" { target *-*-darwin* } }  */

char array[1];

/* { dg-final { scan-assembler-not {\msizetype} } } */
