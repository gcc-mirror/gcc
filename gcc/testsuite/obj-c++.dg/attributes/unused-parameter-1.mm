/* { dg-do compile } */

#include <objc/objc.h>

@interface MyRootClass
{
  Class isa;
}
- (id) method1: (id) argument1;
- (id) method2: (id) __attribute__((unused)) argument1;
- (id) method3: (id) __attribute__((unused)) argument1
   andArgument: (id) argument2;
- (id) method4: (id) __attribute__((unused)) argument1
   andArgument: (id) __attribute__((unused)) argument2;
- (id) method5: (id) argument1
   andArgument: (id) __attribute__ ((unused)) argument2;
- (id) method6: (id) argument1
   andArgument: (id) argument2;
@end

@implementation MyRootClass
- (id) method1: (id) argument1
{
  return nil;
}
- (id) method2: (id) __attribute__((unused)) argument1
{
  return nil;
}
- (id) method3: (id) __attribute__((unused)) argument1
   andArgument: (id) argument2
{
  return nil;
}
- (id) method4: (id) __attribute__((unused)) argument1
   andArgument: (id) __attribute__((unused)) argument2
{
  return nil;
}
- (id) method5: (id) argument1
   andArgument: (id) __attribute__ ((unused)) argument2
{
  return nil;
}
- (id) method6: (id) argument1
   andArgument: (id) argument2
{
  return nil;
}
@end
