#include <objc/objc.h>

@interface MyObject
{
  Class isa;
  float f;
  char a[3];
  int i:2;
  int j:6;
  int k:12;
  char c;
  void *pointer;
}
@end

@implementation MyObject
@end

#include "bf-common.h"

