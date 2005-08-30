/* PR preprocessor/20356 */
/* { dg-do compile } */
/* { dg-options "-I$srcdir/gcc.dg/cpp -I$srcdir/gcc.dg/cpp/inc" } */

#include <pr20356-aux.h>

#ifndef PR20356_H
# error PR20356_H not defined
#endif
#ifndef INC_PR20356_H
# error INC_PR20356_H not defined
#endif

int i;
