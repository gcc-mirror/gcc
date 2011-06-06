// Radar 3926484

// { dg-do compile }

#include "../objc-obj-c++-shared/TestsuiteObject.h"
#include <iostream>

@interface Greeter : TestsuiteObject
- (void) greet: (const char *)msg;
@end

@implementation Greeter
- (void) greet: (const char *)msg { std::cout << msg; }
@end

int
main ()
{
  std::cout << "Hello from C++\n";
  Greeter *obj = [Greeter new];
  [obj greet: "Hello from Objective-C\n"];
}

/* { dg-final { scan-assembler-not "L_objc_msgSend\\\$non_lazy_ptr" } } */
