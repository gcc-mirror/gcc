// { dg-do run  }
// { dg-options "-fshort-enums" }

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
