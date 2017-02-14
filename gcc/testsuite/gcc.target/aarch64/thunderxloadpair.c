/* { dg-do compile } */
/* { dg-options "-O2 -mcpu=thunderx" } */

struct ldp
{
  long long c;
  int a, b;
};


int f(struct ldp *a)
{
  return a->a + a->b;
}


/* We know the alignement of a->a to be 8 byte aligned so it is profitable
   to do ldp. */
/* { dg-final { scan-assembler-times "ldp\tw\[0-9\]+, w\[0-9\]" 1 } } */

