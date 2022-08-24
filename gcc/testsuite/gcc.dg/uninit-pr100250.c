/* PR middle-end/100250 - ICE related to -Wmaybe-uninitialized
   { dg-do compile }
   { dg-options "-O2 -Wall" }
   { dg-require-effective-target alloca } */

extern void f (int D, const int[D], const int[D]);

void g (int D, const int a[D], const int b[D], const int c[D], const int d[D])
{
  int c2[D];

  for (int i = 0; i < D; i++) {

    if (a[i] >= D) __builtin_abort ();
    if (b[i] != d[a[i]]) __builtin_abort ();

    c2[a[i]] = c[i];
  }

  f (D, d, c2);
}

void h (int D, const int d[D])
{
  int a[D];
  int b[D];
  int c[D];

  g (D, a, b, c, d);          // { dg-warning "-Wmaybe-uninitialized" }
}
