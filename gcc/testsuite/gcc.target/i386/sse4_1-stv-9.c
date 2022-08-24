/* { dg-do compile { target int128 } } */
/* { dg-options "-O2 -msse4.1 -mstv -mno-stackrealign" } */

unsigned __int128 a, b;
void rot1()  { a = (b >> 1) | (b << 127); }
void rot4()  { a = (b >> 4) | (b << 124); }
void rot8()  { a = (b >> 8) | (b << 120); }
void rot32() { a = (b >> 32) | (b << 96); }
void rot64() { a = (b >> 64) | (b << 64); }

/* { dg-final { scan-assembler-not "shrdq" } } */
/* { dg-final { scan-assembler "pshufd" } } */
