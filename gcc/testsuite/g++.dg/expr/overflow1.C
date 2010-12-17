#include <limits.h>

enum E {
  A = (unsigned char)-1,	/* OK */
  B = (signed char)UCHAR_MAX,	/* implementation-defined */
  C = INT_MAX+1,     /* undefined (C)/ill-formed (C++) { dg-message "" } */
  D = UINT_MAX+1     /* OK */
};
