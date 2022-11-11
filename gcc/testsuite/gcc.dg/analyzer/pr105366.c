/* { dg-require-effective-target int128 } */
/* { dg-additional-options "-O" } */

extern void bar(int);
extern void baz(void);

typedef unsigned u32;

void
foo(u32 u, __int128 i) {
  baz();
  _Complex int c = i;
  c /= (u32)(__UINTPTR_TYPE__)foo;
  short s = (short)(__UINTPTR_TYPE__)foo;
  u /= (_Complex short)s;
  u32 r = u + c;
  bar(r);
  foo(0, 0); /* { dg-warning "infinite recursion" } */
}
