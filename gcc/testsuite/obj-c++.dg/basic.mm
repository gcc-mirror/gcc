// A basic sanity check for Objective-C++.
// { dg-do run }

#include <objc/Object.h>
#include <iostream>

@interface Greeter : Object
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
