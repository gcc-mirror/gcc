/* { dg-add-options vect_early_break } */
/* { dg-do compile } */
/* { dg-require-effective-target vect_early_break } */
/* { dg-require-effective-target vect_int } */

void abort();
int main1 (short X)
{
  unsigned char a[128];
  unsigned short b[128];
  unsigned int c[128];
  short myX = X;
  int i;
  for (i = 0; i < 128; i++)
    {
      if (a[i] != (unsigned char)myX || b[i] != myX || c[i] != (unsigned int)myX++)
        abort ();
    }
}

/* { dg-final { scan-tree-dump "vectorized 1 loops in function" "vect" { target { ! "x86_64-*-* i?86-*-*" } } } } */
