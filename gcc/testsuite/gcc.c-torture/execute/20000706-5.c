extern void abort(void);
extern void exit(int);

struct baz { int a, b, c; };

struct baz *c;

void bar(int b)
{
  if (c->a != 1 || c->b != 2 || c->c != 3 || b != 4)
    abort();
}

void foo(struct baz a, int b)
{
  c = &a;
  bar(b);
}

int main()
{
  struct baz a;
  a.a = 1;
  a.b = 2;
  a.c = 3;
  foo(a, 4);
  exit(0);
}
