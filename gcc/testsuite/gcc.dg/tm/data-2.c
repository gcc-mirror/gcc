/* { dg-do compile } */
/* { dg-options "-fgnu-tm" } */
/* Test read and write on all basic types.  */

struct S
{
  int x[10];
};

static struct S g;

extern void fill (struct S *);

void f(void)
{
  struct S l;
  fill(&l);

  __transaction_atomic {
    g = l;
  }
}
