/* { dg-do compile } */
/* { dg-require-effective-target strub } */
/* { dg-options "-O2 -fstrub=all -fno-omit-frame-pointer" } */

int a[128];

void
f (void)
{
  for (short i = 0; i < 64; i++)
    a[i] = i;
}
