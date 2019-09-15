/* Test if constant CFStrings may be passed back as ObjC strings.  */
/* Author: Ziemowit Laski  */

/* So far, CFString is darwin-only.  */
/* { dg-do compile { target *-*-darwin* } } */
/* { dg-skip-if "NeXT only" { *-*-* } { "-fgnu-runtime" } { "" } } */
/* { dg-options "-mconstant-cfstrings" } */

#include "../../objc-obj-c++-shared/F-NSObject.h"

@interface Foo: NSObject {
  char *cString;
  unsigned int len;
}
+ (Foo *)description;
@end

@interface Bar: NSObject
+ (Foo *) getString: (int) which;
@end

@implementation Bar
+ (Foo *) getString: (int) which {
  return which? [Foo description]: @"Hello";
}
@end
