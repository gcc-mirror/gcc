extern void abort(void);
extern void exit(int);

int c;

void baz(int *p)
{
  c = *p;
}

void bar(int b)
{
  if (c != 1 || b != 2)
    abort();
}

void foo(int a, int b)
{
  baz(&a);
  bar(b);
}

int main()
{
  foo(1, 2);
  exit(0);
}
