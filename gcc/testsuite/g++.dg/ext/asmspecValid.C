// tests that the asm directive is correctly handled for static fields
// in structures and classes.  This only applies to C++; such
// directives generate errors in C.  Assembler directives for local
// variables should be tested by the C test suite.  
//
// Contributed by Robert Bowdidge (bowdidge@apple.com) 14 Oct 2003

// { dg-do compile }

struct Foo {
  // This should reference a variable called bar
  static int i __asm__("bar"); 
};


class Bar {
public:
  static int i __asm__("theRealI");
  static int j __asm__("theRealJ");
  int boof;
};

class Baz : public Bar { 
public:
   static char *ptr __asm__ ("theRealString");
};

int main (int argc, char **argv) {
  struct Foo myFoo;
  Bar b;
  myFoo.i = 1;
  Foo::i = 2;
  Baz::j = 10;
  Baz::ptr = 0;
  b.i = 1;
  return (b.i);
}


/* { dg-final {scan-assembler "bar"} } */
/* { dg-final {scan-assembler "theRealString"} } */
/* { dg-final {scan-assembler "theRealI" } } */
/* { dg-final {scan-assembler "theRealJ" } } */
