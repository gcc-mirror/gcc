/* { dg-do compile } */
/* { dg-options "-O -mavx512vbmi" } */

int a[1024];

void
foo (int i)
{
  for (;; i++)
    if (a[i] != (i ^ (i * 3) ^ (i * 7)))
      return;
}
