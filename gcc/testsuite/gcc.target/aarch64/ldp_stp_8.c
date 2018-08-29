/* { dg-options "-O2" } */

typedef float __attribute__ ((vector_size (8))) fvec;
typedef int __attribute__ ((vector_size (8))) ivec;

struct pair
{
  double a;
  fvec b;
};

void ldp (double *a, fvec *b, struct pair *p)
{
  *a = p->a + 1;
  *b = p->b;
}

struct vec_pair
{
  fvec a;
  ivec b;
};

void ldp2 (fvec *a, ivec *b, struct vec_pair *p)
{
  *a = p->a;
  *b = p->b;
}

/* { dg-final { scan-assembler-times "ldp\td\[0-9\], d\[0-9\]+, \\\[x\[0-9\]+\\\]" 2 } } */
