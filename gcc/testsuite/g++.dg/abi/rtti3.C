// PR 20647, we must emit the typeinfo's string as weak, but not the
// necessarily the type info object

// { dg-require-weak "" }
// { dg-final { scan-assembler ".weak\[ \t\]_?_ZTSPP1A" } }
// { dg-final { scan-assembler-not ".weak\[ \t\]_?_ZTIPP1A" } }

struct A;

void Foo ()
{
  throw (A **)0;
}
