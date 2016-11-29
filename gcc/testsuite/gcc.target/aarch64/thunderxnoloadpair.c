/* { dg-do compile } */
/* { dg-options "-O2 -mcpu=thunderx" } */

struct noldp
{
  int a, b;
};


int f(struct noldp *a)
{
  return a->a + a->b;
}

/* We know the alignement of a->a to be 4 byte aligned so it is not profitable
   to do ldp. */
/* { dg-final { scan-assembler-not "ldp\tw\[0-9\]+, w\[0-9\]" } } */
