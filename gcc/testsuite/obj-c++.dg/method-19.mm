/* Test if instance methods of root classes are used as class methods, if no
   "real" methods are found.  For receivers of type 'id' and 'Class', all
   root classes must be considered.  */
/* Author: Ziemowit Laski <zlaski@apple.com>.  */
/* { dg-do run } */
/* { dg-xfail-run-if "Needs OBJC2 ABI" { *-*-darwin* && { lp64 && { ! objc2 } } } { "-fnext-runtime" } { "" } } */
// { dg-additional-options "-Wno-objc-root-class" }
#include <objc/objc.h>
#include "../objc-obj-c++-shared/runtime.h"

#include <stdlib.h>
#include <string.h>

#define CHECK_IF(expr) if(!(expr)) abort()

@protocol Proto
- (const char *) method4;
@end

@interface Root
{ Class isa; }
+ (const char *) method2;
@end

@interface Derived: Root
- (const char *) method1;
- (const char *) method2;
- (const char *) method3;
@end

@interface Root (Categ)
- (const char *) method3;
@end

@implementation Root (Categ)
- (const char *) method3 { return "Root(Categ)::-method3"; }
- (const char *) method4 { return "Root(Categ)::-method4"; }
@end

@implementation Derived
- (const char *) method1 { return "Derived::-method1"; }
- (const char *) method2 { return "Derived::-method2"; }
- (const char *) method3 { return "Derived::-method3"; }
@end

@implementation Root
+ initialize { return self; }
- (const char *) method1 { return "Root::-method1"; }
+ (const char *) method2 { return "Root::+method2"; }
@end

int main(void)
{
  Class obj = objc_getClass("Derived");

  /* None of the following should elicit compiler-time warnings.  */

  CHECK_IF(!strcmp([Root method1], "Root::-method1"));
  CHECK_IF(!strcmp([Root method2], "Root::+method2"));
  CHECK_IF(!strcmp([Root method3], "Root(Categ)::-method3"));
  CHECK_IF(!strcmp([Root method4], "Root(Categ)::-method4"));
  CHECK_IF(!strcmp([Derived method1], "Root::-method1"));
  CHECK_IF(!strcmp([Derived method2], "Root::+method2"));
  CHECK_IF(!strcmp([Derived method3], "Root(Categ)::-method3"));
  CHECK_IF(!strcmp([Derived method4], "Root(Categ)::-method4"));
  CHECK_IF(!strcmp([obj method1], "Root::-method1"));
  CHECK_IF(!strcmp([obj method2], "Root::+method2"));
  CHECK_IF(!strcmp([obj method3], "Root(Categ)::-method3"));
  CHECK_IF(!strcmp([obj method4], "Root(Categ)::-method4"));

  return 0;
}

