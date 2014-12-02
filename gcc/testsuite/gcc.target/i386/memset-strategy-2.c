/* PR target/64108 */
/* { dg-do compile } */
/* { dg-options "-O2 -march=atom -mmemset-strategy=libcall:-1:align -minline-all-stringops" } */

char a[2048];
void t (void)
{
  __builtin_memset (a, 1, 2048);
}

