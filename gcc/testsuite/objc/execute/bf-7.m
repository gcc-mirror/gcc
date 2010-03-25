#import "../../objc-obj-c++-shared/Object1.h"
#include <objc/objc.h>
#include <objc/objc-api.h>

@interface MyObject
{
  Class isa;
  float f;
  char a;
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
