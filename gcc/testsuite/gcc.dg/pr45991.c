/* { dg-do compile } */
/* { dg-options "-O -fstrict-overflow" } */

struct S
{
  int i;
};

char A[64];

void foo (char **dst, int i)
{
  char *p = A + 16;
  while (i--)
    {
      int b = ((struct S *) (&p[i * 16 + 4]))->i;
      char *c = A + i * 16;
      dst[i] = c + b;
    }
}
