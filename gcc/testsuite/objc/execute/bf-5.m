#include <objc/objc.h>

@interface MyObject
{
  Class isa;
  float f;
  char a;
  int i:2;
  int j:3;
  int k:12;
  char c;
  void *pointer;
}
@end

@implementation MyObject
@end

#include "bf-common.h"

