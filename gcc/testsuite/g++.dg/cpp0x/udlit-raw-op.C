// { dg-do run { target c++11 } }
// { dg-skip-if "requires hosted libstdc++ for cassert" { ! hostedlib } }

#include <cassert>
#include <cstring>

int
operator ""_raw_umber(const char * str)
{
  return strlen(str);
}

int
main()
{
  int i = 0123012301230123012301230123012301230123012301230123012301230123_raw_umber;	// { dg-warning "octal literals without '0o' or '0O' prefix other than '0' are deprecated" "" { target c++29 } }
  assert( i == 64 );

  int j = 90123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789_raw_umber;
  assert( j == 101 );
}
