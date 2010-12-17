
/* { dg-do compile } */
/* { dg-options "-Wunused-but-set-variable" } */

#import "../objc-obj-c++-shared/Object1.h"
#include <objc/objc-api.h>

@interface obj : Object
{
  int value;
}
- (int) value;
- (void) setValue: (int)number;
@end

@implementation obj : Object

- (int) value { return value; }
- (void) setValue: (int)number { value = number; }

@end

int main (void)
{
  obj *a;		/* { dg-bogus "set but not used" } */
  obj *b;		/* { dg-bogus "set but not used" } */
  obj *c;		/* { dg-warning "set but not used" } */

  a = [obj new];
  b = [obj new];
  c = [obj new];

  [b setValue: [a value]];

  return [a value];
}
