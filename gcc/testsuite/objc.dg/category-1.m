/* Test class methods inside categories.  */
/* Author: Ziemowit Laski <zlaski@apple.com>.  */

/* { dg-do run } */
/* { dg-xfail-run-if "need OBJC2 ABI" { *-*-darwin* && { lp64 &&  { ! objc2 } } } { "-fnext-runtime" } { "" } } */

#include "../objc-obj-c++-shared/TestsuiteObject.m"
extern int strcmp(const char *s1, const char *s2);
extern void abort(void);

#define CHECK_IF(expr) if(!(expr)) abort()

@interface MyObject: TestsuiteObject
+ (Class)whatever1;
@end

@implementation MyObject
+ (Class)whatever1 { return [super superclass]; }
@end

@interface MyObject (ThisWontCompile)
+(Class)whatever2;
@end
 
@implementation MyObject (ThisWontCompile)
+(Class)whatever2 { return [super superclass]; }
@end

int main (int argc, const char * argv[])
{
  Class w1 = [MyObject whatever1];
  Class w2 = [MyObject whatever2];

  CHECK_IF(!strcmp( object_getClassName( w1 ), "TestsuiteObject"));
  CHECK_IF(!strcmp( object_getClassName( w2 ), "TestsuiteObject"));

  return 0;
}

