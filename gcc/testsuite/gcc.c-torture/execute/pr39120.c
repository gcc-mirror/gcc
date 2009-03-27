struct X { int *p; } x;

struct X __attribute__((noinline))
foo(int *p) { struct X x; x.p = p; return x; }

void __attribute((noinline))
bar() { *x.p = 1; }

extern void abort (void);
int main()
{
  int i = 0;
  x = foo(&i);
  bar();
  if (i != 1)
    abort ();
  return 0;
}
