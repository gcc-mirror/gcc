/* Ensure that the compiler does not emit spurious extern declarations named '_Foo', where 'Foo'
   is an ObjC class name.  */
/* Contributed by Ziemowit Laski <zlaski@apple.com>.  */
/* { dg-do run } */

#include <objc/Object.h>
#include <stdlib.h>
#define CHECK_IF(expr) if(!(expr)) abort()

@interface _Child: Object
+ (int) flashCache;
@end

@interface Child: _Child
+ (int) flushCache1;
@end

@interface Child (Categ)
+ (int) flushCache2;
@end

int _Object = 23;  /* Should not conflict with @interface Object.  */

@implementation _Child
+ (int) flashCache { return 12 + _Object; }
@end

@implementation Child
+ (int) flushCache1 { return 7 + [super flashCache]; }
@end

@implementation Child (Categ)
+ (int) flushCache2 { return 9 + [super flashCache]; }
@end

int main(void) {
  CHECK_IF([_Child flashCache] == 35);
  CHECK_IF([Child flushCache1] == 42);
  CHECK_IF([Child flushCache2] == 44);

  return 0; 
}

