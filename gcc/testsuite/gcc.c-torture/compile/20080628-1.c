void f (long double, long double);

struct s {
  char c;
  struct s *p;
} *p;

void
g (void)
{
  long double ld;
  p->p->c = 1;
  ld = p->p->c;
  f (ld, 1.0L);
}
