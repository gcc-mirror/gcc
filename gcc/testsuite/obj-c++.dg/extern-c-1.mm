/* Test extern c support inside @implementation */
/* Devang Patel  <dpatel@apple.com>.  */
// { dg-additional-options "-Wno-objc-root-class" }

#include <objc/objc.h>

@interface Extern 
@end

@implementation Extern

extern "C" void NSRegisterElement(id element);

- init {
  NSRegisterElement(self);
  return self;
}

@end
