// { dg-lto-do link }
// { dg-require-effective-target c++11 }

// Check that the type std::nullptr_t is pretty printed when LTO is involved

#include <cstddef>

std::nullptr_t x; // { dg-lto-message "type 'nullptr_t' should match type 'int'" }

int get (void);

int
main ()
{
  return get ();
}
