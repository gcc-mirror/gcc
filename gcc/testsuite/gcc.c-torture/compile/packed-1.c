struct s
{
  int e;
} x;

struct rbuf
{
  struct s *p __attribute__ ((packed));
} *b;

void
f (void)
{
  b->p = &x;
}
