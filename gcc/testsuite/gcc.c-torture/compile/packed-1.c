struct s
{
  int e;
} x;

struct rbuf
{
  struct s *p __attribute__ ((packed));
} *b;

f ()
{
  b->p = &x;
}
