/* { dg-do compile } */
/* { dg-options "-O3" } */

typedef struct dest
{
  double m[3][3];
} dest;

typedef struct src
{
  int m[3][3];
} src;

void
foo (dest *a, src* s)
{
  for (int i = 0; i != 3; i++)
    for (int j = 0; j != 3; j++)
      a->m[i][j] = s->m[i][j];
}
