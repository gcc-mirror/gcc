// PR ipa/99034
// { dg-do compile }
// { dg-options "-O2" }

void *b[5];
void foo (void);
struct S { ~S (); };

static inline void
__attribute__((always_inline))
bar (int d)
{
  S s;
  while (d)
    foo ();
}

void
baz (void)
{
  bar (2);
  __builtin_setjmp (b);
}
