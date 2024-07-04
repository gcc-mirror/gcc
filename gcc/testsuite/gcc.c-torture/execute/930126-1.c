void abort (void);
void exit (int);

struct s {
  unsigned long long a:8, b:32;
};

struct s
f(struct s x)
{
  x.b = 0xcdef1234;
  return x;
}

int
main(void)
{
  static struct s i;
  i.a = 12;
  i = f(i);
  if (i.a != 12 || i.b != 0xcdef1234)
    abort();
  exit(0);
}
