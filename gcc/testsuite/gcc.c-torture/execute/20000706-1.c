extern void abort(void);
extern void exit(int);

struct baz {
  int a, b, c, d, e;
};

void bar(struct baz *x, int f, int g, int h, int i, int j)
{
  if (x->a != 1 || x->b != 2 || x->c != 3 || x->d != 4 || x->e != 5 ||
      f != 6 || g != 7 || h != 8 || i != 9 || j != 10)
    abort();
}

void foo(struct baz x, char **y)
{
  bar(&x,6,7,8,9,10);
}

int main()
{
  struct baz x;

  x.a = 1;
  x.b = 2;
  x.c = 3;
  x.d = 4;
  x.e = 5;
  foo(x,(char **)0);
  exit(0);
}
