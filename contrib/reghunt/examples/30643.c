extern void abort (void);
struct s {  int a;  int b;};
void bar (struct s *ps,  int *p, int *__restrict__ rp, int *__restrict__ rq)
{
  ps->a = 0;
  ps->b = 1;
  if (ps->a != 0)    abort ();
  p[0] = 0;
  p[1] = 1;
  if (p[0] != 0)     abort ();
  rp[0] = 0;
  rq[0] = 1;
  if (rp[0] != 0)     abort();
}
