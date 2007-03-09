/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-not "undefined" } } */

/* Make sure we optimize all calls away.  */

extern void undefined (void);
struct s { int a, b; };
void bar (struct s *ps,  int *p, int *__restrict__ rp, int *__restrict__ rq)
{
  ps->a = 0;
  ps->b = 1;
  if (ps->a != 0)
    undefined ();
  p[0] = 0;
  p[1] = 1;
  if (p[0] != 0)
    undefined ();
  rp[0] = 0;
  rq[0] = 1;
  if (rp[0] != 0)
    undefined ();
}
int main (void) {
  return 0;
}
