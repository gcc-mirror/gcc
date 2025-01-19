// Test for sibcall optimization with struct aligned on stack.
// { dg-options "-O2" }
// { dg-final { scan-assembler "jmp" { target i?86-*-* x86_64-*-* } } }

struct A { char a[17]; };

int baz (int a, int b, int c, void *p, struct A s, struct A);

int
foo (int a, int b, int c, void *p, struct A s, struct A s2)
{
  return baz (a, b, c, p, s, s2);
}
