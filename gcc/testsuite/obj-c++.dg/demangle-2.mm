/* Test demangling an Objective-C method.  */
/* { dg-do run } */
/* { dg-xfail-run-if "Needs OBJC2 ABI" { *-*-darwin* && { lp64 && { ! objc2 } } } { "-fnext-runtime" } { "" } } */

#include <cstring>
#include <cstdlib>
#include <iostream>
#include <objc/objc.h>

@interface DemangleTest
{
  Class isa;
}
+ (int) testFunction1;
+ (int) test_function2;
+ (int) __testFunction3: (int)unused  andArgument: (char)unused2;
+ (id) initialize;
@end

@implementation DemangleTest
+ (int) testFunction1
{
  std::cout << __PRETTY_FUNCTION__ << "\n";
  return std::strcmp (__PRETTY_FUNCTION__, "+[DemangleTest testFunction1]");
}
+ (int) test_function2
{
  std::cout << __PRETTY_FUNCTION__ << "\n";
  return std::strcmp (__PRETTY_FUNCTION__, "+[DemangleTest test_function2]");
}
+ (int) __testFunction3: (int)unused   andArgument: (char)unused2
{
  std::cout << __PRETTY_FUNCTION__ << "\n";
  return std::strcmp (__PRETTY_FUNCTION__, "+[DemangleTest __testFunction3:andArgument:]");
}
+ (id) initialize { return self; }
@end

int main ()
{
  if ([DemangleTest testFunction1] != 0)
      abort ();

  if ([DemangleTest test_function2] != 0)
      abort ();

  if ([DemangleTest __testFunction3:0 andArgument: 'c'] != 0)
      abort ();
  
  return 0;
}


