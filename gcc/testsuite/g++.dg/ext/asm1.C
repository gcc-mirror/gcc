// Check that the 3.1 named operand syntax can be used in template functions.

struct arg1 {
  int value;
  static const int info = 99;
};

struct arg2 {
  int value;
  static const int info = 11;
};

template<int j>
int foo (void)
{
  int i;
  asm ("# foo on %[third] %[second] %[fourth] %[first]"
       : [first] "=r" (i)
       : [second] "i" (j),
         [third] "i" (j + 2),
         [fourth] "i" (100));
  return i;
}

template<class TYPE>
TYPE bar (TYPE t)
{
  asm ("# bar on %[first] %[second] %[third]"
       : [first] "=r" (t.value)
       : [second] "i[first]" (t.value),
         [third] "i" (t.info));
  return t;
}

template<class TYPE>
struct S {
  static void frob (TYPE t)
  {
    asm ("# frob on %[arg]" :: [arg] "i" (t.info));
  }
};

void test ()
{
  arg1 x;
  arg2 y;

  foo<42> ();
  bar (x);
  bar (y);
  S<arg1>::frob (x);
}

// { dg-final { scan-assembler "foo on" } }
// { dg-final { scan-assembler "bar on" } }
// { dg-final { scan-assembler "frob on" } }
