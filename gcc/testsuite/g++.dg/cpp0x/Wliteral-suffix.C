// { dg-do run }
// { dg-options "-std=c++0x" }

// Make sure -Wliteral-suffix is enabled by default and
// triggers as expected.

#define BAR "bar"
#define PLUS_ONE + 1

#include <cstdint>
#include <cassert>


void
test()
{
  char c = '3'PLUS_ONE;	  // { dg-warning "invalid suffix on literal" }
  char s[] = "foo"BAR;	  // { dg-warning "invalid suffix on literal" }

  assert(c == '4');
  assert(s[3] != '\0');
  assert(s[3] == 'b');
}

int
main()
{
  test();
}
