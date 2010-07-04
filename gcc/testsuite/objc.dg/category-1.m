/* Test class methods inside categories.  */
/* Author: Ziemowit Laski <zlaski@apple.com>.  */

/* { dg-do run } */
/* { dg-xfail-run-if "need OBJC2 ABI" { *-*-darwin* && { lp64 &&  { ! objc2 } } } { "-fnext-runtime" } { "" } } */

#include "../objc-obj-c++-shared/Object1.h"
extern int strcmp(const char *s1, const char *s2);
extern void abort(void);

#ifdef __NEXT_RUNTIME__
#define SUPERCLASS superclass
#else
#define SUPERCLASS superClass
#endif

#define CHECK_IF(expr) if(!(expr)) abort()

@interface MyObject: Object
+ (Class)whatever1;
@end

@implementation MyObject
+ (Class)whatever1 { return [super SUPERCLASS]; }
@end

@interface MyObject (ThisWontCompile)
+(Class)whatever2;
@end
 
@implementation MyObject (ThisWontCompile)
+(Class)whatever2 { return [super SUPERCLASS]; }
@end

int main (int argc, const char * argv[])
{
  Class w1 = [MyObject whatever1];
  Class w2 = [MyObject whatever2];

#ifdef NEXT_OBJC_USE_NEW_INTERFACE
  CHECK_IF(!strcmp( object_getClassName( w1 ), "Object"));
  CHECK_IF(!strcmp( object_getClassName( w2 ), "Object"));
#else
  CHECK_IF(!strcmp(w1->name, "Object"));
  CHECK_IF(!strcmp(w2->name, "Object"));
#endif

  return 0;
}

#include "../objc-obj-c++-shared/Object1-implementation.h"
