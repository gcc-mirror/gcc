// tests that the asm directive is correctly handled for static fields
// in structures and classes.  This only applies to C++; such
// directives generate errors in C.  Assembler directives for local
// variables should be tested by the C test suite.  
//
// Contributed by Robert Bowdidge (bowdidge@apple.com) 14 Oct 2003

// { dg-do compile }

struct Foo {
  // This should reference a variable called bar
  int i __asm__("bar");  /* { dg-error "specifiers are not permitted" } */
};

int main (void ) {
  int j = 0;
  return j;
}
