#include "spec3.h"

extern void func () throw (B,A);

void spec3_x (void)
{
  try {	func(); }
  catch (A& a) { }
}
