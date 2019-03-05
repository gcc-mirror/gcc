/* Test corner case when LG from literal pool could be preferred to LARL.  */

/* { dg-do compile { target { lp64 } } } */
/* { dg-options "-O3 -march=z13" } */

int e = 42;
int *c = &e;

void
h (int *i)
{
  c = i;
}

void
j ()
{
  h (&e);
  /* { dg-final { scan-assembler {(?n)\n\tlarl\t.+\n\tstgrl\t.+\n\tbr\t%r14\n} } } */
}

void
f ()
{
  h (c);
}
