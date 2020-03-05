/* { dg-do compile { target { ! ia32 } } } */
/* { dg-require-effective-target maybe_x32 } */
/* { dg-options "-O2 -mx32 -maddress-mode=long -ftree-vectorize" } */

struct s2{
  int n[24 -1][24 -1][24 -1];
};

struct test2{
  struct s2 e;
};

struct test2 tmp2[4];

void main1 ()
{
  int i,j;

  for (i = 0; i < 24 -4; i++)
      for (j = 0; j < 24 -4; j++)
          tmp2[2].e.n[1][i][j] = 8;
}
