/* { dg-do compile } */
/* { dg-options "-fsanitize=bounds -Wall -Wextra" } */

/* Do not generate invalid diagnostics.  */

extern const int a[10];
extern int bar (int);
void
foo (int i, int j)
{
  bar (a[i] >> j);
  bar ((unsigned long) a[i] >> j);
  bar ((short int) (unsigned long) a[i] >> j);
  bar (j >> a[i]);
  bar (j >> (unsigned long) a[i]);
  bar (j >> (short int) (unsigned long) a[i]);
  bar (a[i] / j);
  bar ((unsigned long) a[i] / j);
  bar ((short int) (unsigned long) a[i] / j);
  bar (j / a[i]);
  bar (j / (unsigned long) a[i]);
  bar (j / (short int) (unsigned long) a[i]);
}
