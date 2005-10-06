/* Test Objective-C method encodings. */

/* The _encoded_ parameter offsets for Objective-C methods are 
   computed inductively as follows:
    - The first paramter (self) has offset 0;
    - The k-th parameter (k > 1) has offset equal to the
      sum of:
        - the offset of the k-1-st paramter
        - the (void *)-promoted size of the k-1-st parameter.

   Note that the encoded offsets need not correspond
   to the actual placement of parameters (relative to 'self')
   on the stack!  Your target's ABI may have very different
   opinions on the matter.  */

/* Contributed by Ziemowit Laski <zlaski@apple.com>.  */
/* { dg-do run } */


#include <objc/objc.h>
#include <objc/Object.h>

#ifdef __NEXT_RUNTIME__
#define METHOD Method
#define OBJC_GETCLASS objc_getClass
#define CLASS_GETINSTANCEMETHOD class_getInstanceMethod
#else
#include <objc/objc-api.h>
#define METHOD Method_t
#define OBJC_GETCLASS objc_get_class
#define CLASS_GETINSTANCEMETHOD class_get_instance_method
#endif

#include <stdio.h>
#include <stdlib.h>

#define CHECK_IF(expr) if(!(expr)) abort()

@interface Foo: Object
typedef struct { float x, y; } XXPoint;
typedef struct { float width, height; } XXSize;
typedef struct _XXRect { XXPoint origin; XXSize size; } XXRect;
-(id)setRect:(XXRect)r withInt:(int)i;
-(void) char:(signed char)c float:(float)f double:(double)d long:(long)l;
@end

XXRect my_rect;
unsigned offs1, offs2, offs3, offs4, offs5, offs6, offs7;

@implementation Foo
-(id)setRect:(XXRect)r withInt:(int)i {
  unsigned offs = sizeof(self);
  CHECK_IF(offs == offs3);
  offs += sizeof(_cmd);
  CHECK_IF(offs == offs4);
  offs += sizeof(r);
  CHECK_IF(offs == offs5);
  offs += sizeof(i); 
  CHECK_IF(offs == offs1); 
  return nil; 
}
-(void) char:(signed char)c float:(float)f double:(double)d long:(long)l {
  unsigned offs = sizeof(self);
  CHECK_IF(offs == offs3);
  offs += sizeof(_cmd);
  CHECK_IF(offs == offs4);
  offs += sizeof((int)c);
  CHECK_IF(offs == offs5);
  offs += sizeof(f);
  CHECK_IF(offs == offs6);
  offs += sizeof(d);
  CHECK_IF(offs == offs7);
  offs += sizeof(l);
  CHECK_IF(offs == offs1);
}
@end


int main(void) {
  Foo *foo = [[Foo alloc] init];
  Class fooClass = OBJC_GETCLASS("Foo");
  METHOD meth;
  const char *string;

  meth = CLASS_GETINSTANCEMETHOD(fooClass, @selector(setRect:withInt:));
  offs2 = 9999;
  sscanf(meth->method_types, "@%u@%u:%u{_XXRect={?=ff}{?=ff}}%ui%u", &offs1, &offs2, &offs3,
      &offs4, &offs5);
  CHECK_IF(!offs2);
  [foo setRect:my_rect withInt:123];

  meth = CLASS_GETINSTANCEMETHOD(fooClass, @selector(char:float:double:long:));
  offs2 = 9999;
  if (sizeof (long) == 8)
    string = "v%u@%u:%uc%uf%ud%uq%u";
  else
    string = "v%u@%u:%uc%uf%ud%ul%u";
  sscanf(meth->method_types, string, &offs1, &offs2, &offs3,  
	 &offs4, &offs5, &offs6, &offs7);
  CHECK_IF(!offs2);
  [foo char:'c' float:2.3 double:3.5 long:2345L];

  return 0;
}  
