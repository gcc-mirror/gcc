/* { dg-do run } */

extern void abort (void);
struct Foo {
    int *p;
} x;
void __attribute__((noinline))
foo()
{
  *x.p = 0;
}
int main()
{
  int b;
  struct Foo g;
  b = 1;
  g.p = &b;
  x = g;
  foo();
  if (b != 0)
    abort ();
  return 0;
}
