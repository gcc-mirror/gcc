#import "../../objc-obj-c++-shared/Object1.h"
#include <objc/objc.h>
#include <objc/objc-api.h>

@interface MyObject
{
  Class isa;
  float f;
  char a[3];
  struct {
    int i:2;
    int j:6;
    char s;
    int k:12;
  } flags;
  char d;
  void *pointer;
}
@end

@implementation MyObject
@end

#include "bf-common.h"

