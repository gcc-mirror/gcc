typedef int __attribute__ ((mode (SI))) int_t;

struct s
{
  int_t n;
  int_t c[];
};

int_t
ashlsi (int_t x, const struct s *s)
{
  int_t i;

  for (i = 0; i < s->n; i++)
    x ^= 1 << s->c[i];
  return x;
}
