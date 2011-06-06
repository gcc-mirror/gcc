// A basic sanity check for Objective-C++.
// { dg-do run }
/* { dg-xfail-run-if "Needs OBJC2 ABI" { *-*-darwin* && { lp64 && { ! objc2 } } } { "-fnext-runtime" } { "" } } */

#include "../objc-obj-c++-shared/TestsuiteObject.m"
#include <iostream>

@interface Greeter : TestsuiteObject
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

