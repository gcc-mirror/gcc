/* Contributed by Nicola Pero - Fri Mar  9 19:39:15 CET 2001 */
#include <objc/objc.h>
#include <objc/Object.h>

/* Test the @private, @protected, @public keyworks for ivars.  We only
   check syntax. */

@interface TestClass : Object
{
  int a;

@private
  int ivarOne, ivarTwo;
  id ivarThree;

@protected
  int ivarFour;

@public
  id ivarFive;
}
@end

@implementation TestClass
@end


int main (void)
{
  /* Only test compilation */
  return 0;
}
