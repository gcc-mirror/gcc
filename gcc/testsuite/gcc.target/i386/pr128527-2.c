/* { dg-do compile } */
/* { dg-options "-O2 -fno-omit-frame-pointer" } */

int a[128];

void
f (char **w)
{
  char *q = (char *) __builtin_stack_address () - 128;
  if (q < *w)
    *w = q;
  for (short i = 0; i < 64; i++)
    a[i] = i;
}
