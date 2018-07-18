// PR c++/60336
// { dg-do run }
// { dg-options "-Wabi=11" }

struct S
{
  struct { } a;
  __extension__ int b[];
};

struct S s;
struct S a[5];

void
foo (struct S, struct S *arg1, struct S) // { dg-warning "ABI" "" { target { { i?86-*-* x86_64-*-* } && { ! { ia32 } } } } }
{
  if (arg1 != &a[1])
    __builtin_abort ();
}

int
main ()
{
  foo (s, &a[1], a[2]); // { dg-warning "ABI" "" { target { { i?86-*-* x86_64-*-* } && { ! { ia32 } } } } }
}
