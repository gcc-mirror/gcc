/* Make sure that array arguments to methods are given the size of pointers.  */
/* As in the case of ivars, arrays without size (e.g., 'int []') are
   encoded as pointers.  */
/* Contributed by Ziemowit Laski <zlaski@apple.com>.  */
/* { dg-do run } */

#include <objc/objc.h>
#ifdef __NEXT_RUNTIME__
#include <objc/objc-runtime.h>
#define OBJC_GETCLASS objc_getClass
#define CLASS_GETINSTANCEMETHOD class_getInstanceMethod
#else
#include <objc/objc-api.h>
#define OBJC_GETCLASS objc_get_class
#define CLASS_GETINSTANCEMETHOD class_get_instance_method
#endif

extern int sscanf(const char *str, const char *format, ...);
extern void abort(void);
#define CHECK_IF(expr) if(!(expr)) abort()

enum Enum { one, two, three, four };

@interface ArrayTest
- (const char *)str:(signed char [])arg1 with:(unsigned char *)arg2 and:(enum Enum[4])en;
- (int)meth1:(int [])arg1 with:(int [0])arg2 with:(int [2])arg3;
@end

@implementation ArrayTest
- (int)meth1:(int [])arg1 with:(int [0])arg2 with:(int [2])arg3 { return 0; }
- (const char *)str:(signed char [])arg1 with:(unsigned char *)arg2 and:(enum Enum[4])en { return "str"; }
@end

Class cls;
struct objc_method *meth;
unsigned totsize, offs0, offs1, offs2, offs3, offs4, offs5, offs6, offs7;

static void scan_initial(const char *pattern) {
  totsize = offs0 = offs1 = offs2 = offs3 = offs4 = offs5 = offs6 = offs7 = (unsigned)-1;
  sscanf(meth->method_types, pattern, &totsize, &offs0, &offs1, &offs2, &offs3,
      &offs4, &offs5, &offs6, &offs7);
  CHECK_IF(!offs0 && offs1 == sizeof(id) && offs2 == offs1 + sizeof(SEL) && totsize >= offs2);
}

int main(void) {
  cls = OBJC_GETCLASS("ArrayTest");

  meth = CLASS_GETINSTANCEMETHOD(cls, @selector(str:with:and:));
  scan_initial("r*%u@%u:%u*%u*%u[4i]%u");
  CHECK_IF(offs3 == offs2 + sizeof(signed char *) && offs4 == offs3 + sizeof(unsigned char *));
  CHECK_IF(totsize == offs4 + sizeof(enum Enum *));
  meth = CLASS_GETINSTANCEMETHOD(cls, @selector(meth1:with:with:));
  scan_initial("i%u@%u:%u^i%u[0i]%u[2i]%u");
  CHECK_IF(offs3 == offs2 + sizeof(int *) && offs4 == offs3 + sizeof(int *));
  CHECK_IF(totsize == offs4 + sizeof(int *));                                           
  return 0;
}

