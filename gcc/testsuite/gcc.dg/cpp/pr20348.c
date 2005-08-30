/* PR preprocessor/20348 */
/* { dg-do compile } */
/* { dg-options "-I$srcdir/gcc.dg/cpp -I$srcdir/gcc.dg/cpp/inc" } */

#include <pr20348-aux.h>
#define MIDDLE
#include <pr20348.h>

#ifndef PR20348_H_SEEN
# error pr20348.h not included after MIDDLE definition
#endif
#ifndef INC_PR20348_H_SEEN
# error inc/pr20348.h not included before MIDDLE definition
#endif

int i;
