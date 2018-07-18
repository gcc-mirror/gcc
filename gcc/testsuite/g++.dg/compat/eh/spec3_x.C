#include "spec3.h"

extern void func ()
#if __cplusplus < 201103L
throw (B,A)
#endif
;

void spec3_x (void)
{
  try {	func(); }
  catch (A& a) { }
}
