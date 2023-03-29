// { dg-do compile }
// { dg-options "-mavx512vbmi -O1 -ftree-loop-vectorize" }

void
foo (_Float16 *r, short int *a)
{
  int i;

  for (i = 0; i < 32; ++i)
    r[i] = !!a[i];
}
