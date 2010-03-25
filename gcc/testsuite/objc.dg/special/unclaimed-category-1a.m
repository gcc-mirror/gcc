/* Contributed by Nicola Pero - Fri Dec 14 08:36:00 GMT 2001 */

/* Test loading unclaimed categories - categories of a class defined
   separately from the class itself.  */

#include "unclaimed-category-1.h"

@implementation TestClass (A)
- (int)A
{
  return 1;
}
@end

@implementation TestClass (B)
- (int)B
{
  return 2;
}
@end

@implementation TestClass (C)
- (int)C
{
  return 3;
}
@end


