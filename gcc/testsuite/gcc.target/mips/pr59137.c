/* { dg-do run } */
/* { dg-options "-mno-plt" } */

extern void abort (void);

struct lispstruct
{
  int e;
  int t;
};

struct lispstruct Cnil_body;
struct lispstruct Ct_body;
int nvalues;

struct lispstruct * __attribute__ ((noinline))
fLlistp (struct lispstruct *x0)
{
  if (x0 == &Cnil_body
      || (((unsigned long) x0 >= 0x80000000) ? 0
	  : (!x0->e ? (x0 != &Cnil_body) : x0->t)))
    x0 = &Ct_body;
  else
    x0 = &Cnil_body;
  nvalues = 1;
  return x0;
}

int main ()
{
  if (fLlistp ((struct lispstruct *) 0xa0000001) != &Cnil_body)
    abort ();
  return 0;
}
