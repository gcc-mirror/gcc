// { dg-do run  }
// { dg-options "-fshort-enums" }

// On ARM EABI targets this testcase will cause a warning to be emitted
// whilst EABI attributes are being merged at link time unless
// the --no-enum-size-warning option is passed to the linker.  Whilst the
// enum-size attributes should only be emitted if there are values of
// enum type that can escape the compilation unit, gcc cannot currently
// detect this; if this facility is added then this linker option should
// not be needed.  arm-*-linux*eabi* should be a good approximation to
// those platforms where the EABI supplement defines enum values to be
// 32 bits wide.
// { dg-options "-fshort-enums -Wl,--no-enum-size-warning" { target arm*-*-linux*eabi* } }

#include <limits.h>

enum A { a1 = 0x7fffffff };
enum B { b1 = 0x80000000 };
enum C { c1 = -1, c2 = 0x80000000 };
enum D { d1 = CHAR_MIN, d2 = CHAR_MAX };
enum E { e1 = CHAR_MIN, e2 = CHAR_MIN };

main()
{
  return (sizeof (A) != 4 || sizeof (B) != 4 || sizeof (C) != 8
	  || sizeof (D) != 1 || sizeof (E) != 1);
}
