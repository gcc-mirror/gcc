struct A {
  unsigned int a, b, c;
};

extern void abort (void);
extern void exit (int);

struct A bar (void)
{
  return (struct A) { 176, 52, 31 };
}

void baz (struct A *a)
{
  if (a->a != 176 || a->b != 52 || a->c != 31)
    abort ();
}

int main ()
{
  struct A d;

  d = ({ ({ bar (); }); });
  baz (&d);
  exit (0);
}
