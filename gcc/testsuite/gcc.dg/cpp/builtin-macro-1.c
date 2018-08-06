/* Origin PR preprocessor/64803

   This test ensures that the value the __LINE__ macro expands to is
   constant and corresponds to the line of the macro expansion point
   the function-like macro expansion it's part of.

   { dg-do run }
   { do-options -no-integrated-cpp }  */

#include <assert.h>

#define C(a, b) a ## b
#define L(x) C(L, x)
#define M(a) int L(__LINE__) = __LINE__; assert(L(__LINE__) == __LINE__);

int
main()
{
  M(a
    );

  assert(L19 == 19);		/* 19 is the line number of the
				   macro expansion point of the
				   invocation of the M macro.  Please
				   adjust in case the layout of this
				   file changes.  */
  return 0;
}
