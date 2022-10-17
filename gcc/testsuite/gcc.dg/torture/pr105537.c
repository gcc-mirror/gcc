/* { dg-do compile } */
/* { dg-additional-options "-ffast-math -fsignaling-nans -fvar-tracking-assignments -fno-move-loop-stores -ftree-loop-distribution" } */

int n;

double
ext1 (int);

void
ext2 (double);

int
sum (int v1, int v2)
{
  return v1 + v2;
}

void
bar (void)
{
  ext2 (ext1 (n));
}

__attribute__ ((optimize ("-O3"))) void
foo (int *x)
{
  static int i;

  bar ();
  for (i = 0; i != 2; i = sum (i, 1))
    n = *x = 0;
}

/* { dg-message "other options take precedence" "" { target *-*-* } 0 } */
