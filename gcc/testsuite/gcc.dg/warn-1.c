/* The compiler used to say passing arg 0 of, which is wrong.  */
/* Radar 3069179 */

/* { dg-options "-O3" } */

static void foo (p)
     int p;
{	/* { dg-warning "passing arg 1 of" } */
}

void bar (void)
{
  void *vp;

  foo (vp);	/* { dg-warning "" } */
}
