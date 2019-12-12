// PR ipa/63838
// { dg-do run }
// { dg-options "-O2 -fdump-ipa-pure-const" }
// { dg-final { scan-ipa-dump-not "Function found to be nothrow: void foo" "pure-const" } }
// { dg-final { scan-ipa-dump-not "Function found to be nothrow: void bar" "pure-const" } }

__attribute__((noinline, noclone)) static void bar (int);
volatile int v;
void (*fn) ();
struct S { S () { v++; } ~S () { v++; } }; // { dg-warning "deprecated" "" { target c++2a } }

__attribute__((noinline, noclone)) static void
foo (int x)
{
  v++; // { dg-warning "deprecated" "" { target c++2a } }
  if (x == 5)
    bar (x);
}

__attribute__((noinline, noclone)) static void
bar (int x)
{
  v++; // { dg-warning "deprecated" "" { target c++2a } }
  if (x == 6)
    foo (x);
  else if (x == 5)
    fn ();
}

__attribute__((noinline, noclone)) void
baz (int x)
{
  S s;
  foo (x);
}

void
throw0 ()
{
  throw 0;
}

int
main ()
{
  fn = throw0;
  asm volatile ("" : : : "memory");
  try
    {
      baz (5);
    }
  catch (int)
    {
    }
}
