/* { dg-do compile } */
/* { dg-options "-O3 -mlasx" } */
/* { dg-final { scan-assembler "xvilvl.d" } } */
/* { dg-final { scan-assembler "xvilvh.d" } } */

#define CMUL(a, b, c)                                                         \
  {                                                                           \
    (c).ai = (a).ai * (b).ai - (a).bi * (b).bi;                               \
    (c).bi = (a).ai * (b).bi + (a).bi * (b).ai;                               \
    (c).ci = (a).ci * (b).ci - (a).di * (b).di;                               \
    (c).di = (a).ci * (b).di + (a).di * (b).ci;                               \
  }
#define CSUM(a, b)                                                            \
  {                                                                           \
    (a).ai += (b).ai;                                                         \
    (a).bi += (b).bi;                                                         \
    (a).ci += (b).ci;                                                         \
    (a).di += (b).di;                                                         \
  }

typedef struct
{
  double ai;
  double bi;
  double ci;
  double di;
} complex;

typedef struct
{
  complex e[6][6];
} matrix;

typedef struct
{
  complex c[6];
} vector;

void
mult_adj_mat_vec (matrix *a, vector *b, vector *c)
{
  register int i, j;
  register complex x, y;
  for (i = 0; i < 6; i++)
    {
      x.ai = x.bi = x.ci = x.di = 0.0;
      for (j = 0; j < 6; j++)
        {
          CMUL (a->e[j][i], b->c[j], y);
          CSUM (x, y);
        }
      c->c[i] = x;
    }
}
