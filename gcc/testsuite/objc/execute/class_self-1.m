/* Contributed by Nicola Pero - Fri Oct 26 22:39:32 BST 2001 */
#include <objc/objc.h>

/* Test calling a class method when there is an instance method 
   with conflicting types */

/* This class should be unused but on broken compilers its instance
   method might get picked up and used instead of the class method of
   another class ! */
struct d
{
  int a;
};

@interface UnusedClass
{
  Class isa;
}
- (struct d) method;
@end

@implementation UnusedClass
- (struct d) method
{
  struct d u;
  u.a = 0;
  
  return u;
}
@end

/* The real class */
@interface TestClass
{
  Class isa;
}
+ (void) test;
+ (int) method;
@end

@implementation TestClass
+ (void) test
{
  if ([self method] != 4)
    {
      abort ();
    }
}

+ (int) method
{
  return 4;
}
#ifdef __NEXT_RUNTIME__                                   
+ initialize { return self; }
#endif
@end


int main (void)
{
  [TestClass test];

  return 0;
}
