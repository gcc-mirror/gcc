// { dg-do compile { target i?86-*-cygwin* i?86-*-mingw* x86_64-*-mingw* } }
//  Mark class static members as dllimport.

struct Baz
{
  Baz(int a_ =0) : a(a_) {}
  int a;
};

class  __attribute__ ((dllimport)) Bar
{
  public:
    static const int two = 2;
    static const int three;
    static const Baz null_baz;
};

int foo()
{
  Bar foobar;
  const int* baz = &Bar::two; 
  int a = foobar.two;
  int b = foobar.three;
  int c = foobar.null_baz.a;
  return (a + b + c + *baz);
}

// { dg-final { scan-assembler __imp\[_\]*__ZN3Bar3twoE } }
// { dg-final { scan-assembler __imp\[_\]*__ZN3Bar5threeE } }
// { dg-final { scan-assembler __imp\[_\]*__ZN3Bar8null_bazE } }
