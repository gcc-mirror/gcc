// A basic sanity check for Objective-C++.
// { dg-do run }
/* { dg-xfail-run-if "Needs OBJC2 ABI" { *-*-darwin* && { lp64 && { ! objc2 } } } { "-fnext-runtime" } { "" } } */

//#include "../objc-obj-c++-shared/Object1.h"
#include <iostream>

#include <objc/Object.h>

#ifdef __OBJC2__
@interface Object (ADDITIONS)
+ initialize;
- init;
+ alloc;
+ new;
@end
@implementation Object (ADDITIONS)
+ initialize  { return self; }
- init  { return self; }
+ alloc { return class_createInstance (self, 0); }
+ new  { return [[self alloc] init]; }
@end
#endif

@interface Greeter : Object
- (void) greet: (const char *)msg;
@end

@implementation Greeter
- (void) greet: (const char *)msg 
{ std::cout << msg << __VERSION__ << std::endl;}
@end

int
main ()
{
  std::cout << "Hello from C++" << __VERSION__ << std::endl;
  Greeter *obj = [Greeter new];
  [obj greet: "Hello from Objective-C++"];
}
//#include "../objc-obj-c++-shared/Object1-implementation.h"
