/* { dg-do compile { target int128 } } */
/* { dg-options "-Oz" } */
unsigned __int128 foo(unsigned __int128 x) { return x >> 1; }
__int128 bar(__int128 x) { return x >> 1; }
/* { dg-final { scan-assembler-times "rcrq" 2 } } */
/* { dg-final { scan-assembler-not "shrdq" } } */
