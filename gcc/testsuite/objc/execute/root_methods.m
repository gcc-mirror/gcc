/* Contributed by Nicola Pero - Thu Mar  8 16:27:46 CET 2001 */

#import "../../objc-obj-c++-shared/runtime.h"
#import <objc/objc.h>

/* Test that instance methods of root classes are available as class 
   methods to other classes as well */

@interface RootClass
{
  Class isa;
}
- (id) self;
@end

@implementation RootClass
- (id) self
{
  return self;
}
+ initialize { return self; }
@end

@interface NormalClass : RootClass
@end

@implementation NormalClass : RootClass
@end

int main (void)
{
  Class normal = objc_getClass ("NormalClass");

  if (normal == Nil)
    {
      abort ();
    }

  if ([NormalClass self] != normal)
    {
      abort ();
    }

  return 0;
}
