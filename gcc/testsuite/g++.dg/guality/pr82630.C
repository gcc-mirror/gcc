// PR debug/82630
// { dg-do run }
// { dg-additional-options "-fPIC" { target fpic } }

struct C
{
  int &c;
  long d;
  __attribute__((always_inline)) C (int &x) : c(x), d() {}
};
int v;

__attribute__((noipa)) void
fn1 (const void *x)
{
  asm volatile ("" : : "g" (x) : "memory");
}

__attribute__((noipa)) void
fn2 (C x)
{
  int a = x.c + x.d;
  asm volatile ("" : : "g" (a) : "memory");
}

__attribute__((noipa)) void
fn3 (void)
{
  asm volatile ("" : : : "memory");
}

__attribute__((noipa))
#ifdef __i386__
__attribute__((regparm (2)))
#endif
static void
fn4 (int *x, const char *y, C z)
{
  fn2 (C (*x));
  fn1 ("baz");
  fn2 (z);	// { dg-final { gdb-test 41 "y\[0\]" "'f'" } }
  fn1 ("baz");	// { dg-final { gdb-test 41 "y\[1\]" "'o'" } }
}

__attribute__((noipa)) void
fn5 (int *x)
{
  fn4 (x, "foo", C (*x));
  fn3 ();
}

int
main ()
{
  int a = 10;
  fn5 (&a);
  return 0;
}
