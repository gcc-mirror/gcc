/* { dg-do compile } */
/* { dg-options "-O2" } */

int y;
extern int foo (int, ...) __attribute__((pure));

int
zzz (char *s1, char *s2, int len, int *q, int c)
{
  int z = 5;
  unsigned int i, b;
  struct s { char a[z]; };
  struct s x;
  int y_tmp = 5;

  for (i = 0; i < len; i++)
    s1[i] = s2[i];

  b = z & 0x3;

  len += (b == 0 ? 0 : 1) + z;

  *q = len;

 if (c)
   y_tmp = foo (z, x, x) + 4;

 z = foo (z, x, x) + 4;
 y = y_tmp;

 return  z;
}
