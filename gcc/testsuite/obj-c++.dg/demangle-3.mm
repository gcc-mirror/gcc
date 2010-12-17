/* Test demangling an Objective-C method in error messages.  */
/* { dg-do compile } */

#include <objc/objc.h>

@interface DemangleTest
{
  Class isa;
}
+ (int) testFunction1;
@end

@implementation DemangleTest
+ (int) testFunction1
{
  /* TODO: Hack the testsuite so we can test that we get 
     dg-error "In function .+[DemangleTest testFunction1]."
     At the moment, the message is filtered out.  */
  z; /* { dg-error "was not declared" } */
}
@end
