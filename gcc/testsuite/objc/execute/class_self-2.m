/* Contributed by Nicola Pero - Fri Oct 26 22:39:32 BST 2001 */
#include <objc/objc.h>

/* Test calling a class method on self where self has been redefined
   to be another class - the call requires a cast */


/* The first class */
struct d
{
  int a;
};

@interface ClassA
{
  Class isa;
}
+ (Class) class;
+ (struct d) method;
@end

@implementation ClassA
+ (Class) class
{
  return self;
}

+ (struct d) method
{
  struct d u;
  u.a = 5;
  
  return u;
}
#ifdef __NEXT_RUNTIME__                                   
+ initialize { return self; }
#endif
@end

/* The second class */
@interface TestClass
{
  Class isa;
}
+ (void) test;
@end

@implementation TestClass
+ (void) test
{
  self = [ClassA class];
  

  if ([(Class)self method].a != 5)
    {
      abort ();
    }
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
