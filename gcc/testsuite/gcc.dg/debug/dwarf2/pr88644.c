/* PR debug/88644 */
/* { dg-do compile } */
/* { dg-options "-gdwarf-4 -dA -gpubnames" } */

char array[1];

/* { dg-final { scan-assembler-not {\msizetype} } } */
