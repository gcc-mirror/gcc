// { dg-do compile }
// { dg-options "-O2 -Wabi=11" }

struct dummy { struct{} a[7][3]; };

extern void test1 (struct dummy, ...);
extern void (*test2) (struct dummy, ...);

void
foo ()
{
  struct dummy a0;
  test1 (a0, 42); // { dg-warning "ABI" "" { target { { i?86-*-* x86_64-*-* } && { ! { ia32 } } } } }
  test2 (a0, 42); // { dg-warning "ABI" "" { target { { i?86-*-* x86_64-*-* } && { ! { ia32 } } } } }
}
