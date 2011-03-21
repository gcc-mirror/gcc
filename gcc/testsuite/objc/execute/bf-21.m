#include <objc/objc.h>

typedef enum 
{
  A, B
} enumeration;

@interface MyObject
{
  enumeration g:4;
}
@end

@implementation MyObject
@end

#include "bf-common.h"

