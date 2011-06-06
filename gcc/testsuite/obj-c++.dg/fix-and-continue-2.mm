/* Static variables, even if local, require indirect access through a stub
   if -mfix-and-continue is enabled.  */

/* Author: Ziemowit Laski <zlaski@apple.com> */
   
/* { dg-do assemble { target *-*-darwin* } } */
/* { dg-options "-mfix-and-continue" } */

#include "../objc-obj-c++-shared/TestsuiteObject.h"

@interface Foo: TestsuiteObject
+ (TestsuiteObject *)indexableFileTypes;
@end

@implementation Foo
+ (TestsuiteObject *)indexableFileTypes
{
  static TestsuiteObject *fileTypes = 0;
  if(!fileTypes) {
    fileTypes = [TestsuiteObject new];
  }
  return fileTypes;
}
@end
