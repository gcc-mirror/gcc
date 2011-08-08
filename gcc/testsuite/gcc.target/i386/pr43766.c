/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-options "-O2 -msse -mregparm=3" { target ia32 } } */

void p (int *a, int i)
{
  __builtin_prefetch (&a[i]);
}

/* { dg-final { scan-assembler-not "lea\[lq\]?\[ \t\]" } } */
