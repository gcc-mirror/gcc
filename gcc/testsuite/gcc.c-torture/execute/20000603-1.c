struct s1 { double d; };
struct s2 { double d; };

double f(struct s1 *a, struct s2 *b)
{
  a->d = 1.0;
  return b->d + 1.0;
}

int main()
{
  struct s1 a;
  a.d = 0.0;
  if (f (&a, (struct s2 *)&a) != 2.0)
    abort ();
  return 0;
}
