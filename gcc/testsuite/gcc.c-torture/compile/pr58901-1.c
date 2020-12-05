typedef signed int __attribute__ ((mode (SI))) int_t;

struct s
{
  int_t n;
  int_t m : 1;
  int_t l : 31;
};

int_t
movdi (int_t x, const struct s *s)
{
  int_t i;

  for (i = 0; i < x; i++)
    {
      const struct s t = s[i];
      x += t.m ? 1 : 0;
    }
  return x;
}
