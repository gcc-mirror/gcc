/* Test calling super from within a category class method.  */
/* Author: Ziemowit Laski <zlaski@apple.com>  */
/* { dg-do compile } */

typedef struct objc_object { struct objc_class *isa; } *id;

@interface NSObject
+ (int) test_func0;
@end
@interface NSMenuItem: NSObject
+ (int) test_func0;
@end

@implementation NSObject
+ (int) test_func0
{}
@end

@implementation NSMenuItem
+ (int) test_func0
{
  return [super test_func0];
}
@end

@interface NSObject (Test)
+ (int) test_func;
@end

@implementation NSObject (Test)
+ (int) test_func
{}
@end

@interface NSMenuItem (Test)
+ (int) test_func;
@end

@implementation NSMenuItem (Test)
+ (int) test_func
{
   return [super test_func];  /* { dg-bogus "dereferencing pointer to incomplete type" } */
}
@end
