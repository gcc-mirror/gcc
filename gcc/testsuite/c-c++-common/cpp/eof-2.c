/* PR preprocess/95183  */

/* { dg-do preprocess } */

#define f(x) x

#include "eof-2.h"
 /* { dg-regexp {[^\n]*eof-2.h:4: error: unterminated argument list invoking macro "f"\n} } */
