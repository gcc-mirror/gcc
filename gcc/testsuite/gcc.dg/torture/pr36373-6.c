/* { dg-do run } */
/* { dg-options "-fno-tree-sra" } */

extern void abort (void);
struct Foo {
    int *p;
} x;
struct Foo __attribute__((noinline))
bar(int *p)
{
  struct Foo f;
  f.p = p;
  return f;
}
void __attribute__((noinline))
foo()
{
  *x.p = 0;
}
int main()
{
  int b;
  b = 1;
  struct Foo g = bar (&b);
  x = g;
  foo();
  if (b != 0)
    abort ();
  return 0;
}
