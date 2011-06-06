/* Check if sending messages to super does not interfere with sending messages
   to classes. */
/* Contributed by Ziemowit Laski <zlaski@apple.com>.  */
/* { dg-options "" } */
/* { dg-do run } */
/* { dg-xfail-run-if "Needs OBJC2 ABI" { *-*-darwin* && { lp64 && { ! objc2 } } } { "-fnext-runtime" } { "" } } */

#include "../objc-obj-c++-shared/TestsuiteObject.m"

extern void abort(void);
#define CHECK_IF(expr) if(!(expr)) abort()

@interface Base: TestsuiteObject
+ (int) class_func1;
- (int) instance_func1;
@end

@interface Derived: Base
+ (int) class_func1;
@end

@interface Derived (Categ)
- (int) instance_func1;
@end

@implementation Base
+ (int) class_func1 { return 234; }
- (int) instance_func1 { return 345; }
@end

@implementation Derived
+ (int) class_func1 { 
  int i = [super class_func1];
  i += [Base class_func1];
  return i;
}
@end

@implementation Derived (Categ)
- (int) instance_func1 { 
  int i = [super instance_func1];
  i += [Base class_func1];  /* { dg-bogus "invalid receiver type" } */
  return i;
}
@end

int main(void) {
  Base *base = [[Base alloc] init];  /* { dg-bogus "invalid receiver type" } */
  Derived *derived = [[Derived alloc] init];
  CHECK_IF([Base class_func1] == 234);  /* { dg-bogus "invalid receiver type" } */
  CHECK_IF([Derived class_func1] == 234 + 234);
  CHECK_IF([base instance_func1] == 345);
  CHECK_IF([derived instance_func1] == 234 + 345);
  return 0;
}

