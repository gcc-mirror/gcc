/* Make sure Objective-C++ can distinguish ObjC classes from C++ classes.  */
/* Author: Ziemowit Laski  <zlaski@apple.com> */

/* { dg-do compile } */

#include "../objc-obj-c++-shared/TestsuiteObject.h"
#include <iostream>
#include <string>

@interface iostream: TestsuiteObject
@end

int main(void) {
  id i = [std::iostream new];  /* { dg-error "not an Objective\\-C class name or alias" } */
  i = [iostream new];
  i = [std::basic_string<char> new];  /* { dg-error "not an Objective\\-C class name or alias" } */

  return 0;
}
