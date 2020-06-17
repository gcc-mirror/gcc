/* This ICE'd before f9e1ea10e657af9fb02fafecf1a600740fd34409 because
   a doloop pattern with a dead set of the iteration variable was
   generated and s390_fix_long_loop_prediction then failed to
   recognize it as branch on count pattern.  */

/* { dg-do compile } */
/* { dg-options "-O3 -march=z10" } */

int a, d, e, f;
long b;
long *volatile c;
void
fn1() {
  for (; e; ++e)
    if (d)
      ;
    else {
      a = 0;
      for (; a != 14; ++a)
	*c = b && f;
      d = 8;
    }
}
