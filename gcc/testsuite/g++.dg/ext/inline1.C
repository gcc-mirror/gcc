// { dg-do compile }
// { dg-options "-O" }
// Make sure inlined non-outlined functions aren't marked weak.
// We'd get a ".weak xyzzy" annotation trigged by the second declaration.

// { dg-final { scan-assembler-not "weak\[^ \t\]*\[ \t\]_?xyzzy" } } 

// The next check isn't really part of the actual test, just to make
// sure there's no outline-copy of xyzzy, because if that really
// happened, it *should* be marked linkonce or perhaps weak.
// { dg-final { scan-assembler-not "xyzzy" } } 

extern int x;
extern void foo(void);
extern void bar(void);

extern "C" inline int xyzzy(int a)
{
  foo();
  return a + x;
}

extern "C" int xyzzy(int);

extern inline int plugh(int c)
{
  return xyzzy (c);
}

int y;
void doit(int b)
{
  y = xyzzy (b) + plugh (b);
}
