#include <objc/objc.h>
#include <objc/objc-api.h>
#include <objc/Object.h>

struct A {
  int i;
  float f;
  int a:3;
  int b:2;
};

@interface MyObject
{
  Class isa;
  int i;
  float f[3];
  struct A a, b;
  char c;
}
@end

@implementation MyObject
@end

#include "bf-common.h"

