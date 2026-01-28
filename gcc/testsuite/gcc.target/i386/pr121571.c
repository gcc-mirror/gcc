/* { dg-do compile } */
/* { dg-options "-march=skylake-avx512 -Os" } */

int a, b, c, d, e, f;

void
foo ()
{
  int g[2] = { e, f };
  if ((g[0] | g[1]) != 0)
    __asm__ (""
             : "=r" (d), "=&r" (c), "=&r" (b), "=&r" (a)
             : "0" (0), "g" (g), "g" (g[1]), "g" (g[0]));
}
