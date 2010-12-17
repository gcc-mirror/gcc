/* Test demangling an Objective-C method.  */
/* { dg-do run } */
/* { dg-xfail-run-if "Needs OBJC2 ABI" { *-*-darwin* && { lp64 && { ! objc2 } } } { "-fnext-runtime" } { "" } } */

#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#include <objc/objc.h>

@interface DemangleTest
{
  Class isa;
}
+ (id) initialize;
+ (int) testFunction1;
+ (int) test_function2;
+ (int) __testFunction3: (int)unused  andArgument: (char)unused2;
@end

@implementation DemangleTest
+ (id) initialize { return self; }
+ (int) testFunction1
{
  printf ("%s\n", __PRETTY_FUNCTION__);
  return strcmp (__PRETTY_FUNCTION__, "+[DemangleTest testFunction1]");
}
/* Note that in general, due to how mangling is done, it's impossible
   to get the demangling right for all functions containing '_' in the
   name.  But at least we should be able to get that right for single
   argument ones that don't end with '_', such as the following
   one.  */
+ (int) test_function2
{
  printf ("%s\n", __PRETTY_FUNCTION__);
  return strcmp (__PRETTY_FUNCTION__, "+[DemangleTest test_function2]");
}
+ (int) __testFunction3: (int)unused   andArgument: (char)unused2
{
  printf ("%s\n", __PRETTY_FUNCTION__);
  return strcmp (__PRETTY_FUNCTION__, "+[DemangleTest __testFunction3:andArgument:]");
}
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


