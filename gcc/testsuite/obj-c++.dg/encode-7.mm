/* Check if array arguments of ObjC methods are decayed to pointer types
   in a proper fashion:
     (1) The _encodings_ for the array arguments should remain to be '[4i]' and
         such, since this has been the case since at least gcc 3.3.
     (2) However, when building the static C functions out of ObjC method signatures,
         we need to decay the arrays into pointers (as C does).
     (3) If array size is not known (e.g., 'int a[]'), then the type shall be
         encoded as a pointer.  */

/* Contributed by Alexander Malmberg <alexander@malmberg.org>  */

#include "../objc-obj-c++-shared/Object1.h"
#include "../objc-obj-c++-shared/next-mapping.h"
#include <stdlib.h>
#include <stdio.h>
#define CHECK_IF(expr) if(!(expr)) abort()

#ifdef __NEXT_RUNTIME__
#define METHOD Method
#else
#include <objc/objc-api.h>
#define METHOD Method_t
#define method_get_types(M) (M)->method_types
#endif

@interface Test : Object
{ float j; }
-(void) test2: (int [5])a with: (int [])b;
-(id) test3: (Test **)b; /* { dg-warning "previous declaration of .\\-\\(id\\)test3:\\(Test \\*\\*\\)b." } */
@end

@implementation Test
-(void) test2: (int [5])a with: (int [])b
{
  a[3] = *b;
}
-(void) test3: (Test [3][4])b {  /* { dg-warning "conflicting types for .\\-\\(void\\)test3:\\(Test \\\[3\\\]\\\[4\\\]\\)b." } */
}
@end

int bb[6] = { 0, 1, 2, 3, 4, 5 };
int *b = bb;
Test *cc[4];
Test **c = cc;

int offs1, offs2, offs3, offs4, offs5, offs6;

int main(int argc, char **argv)
{
  Class testClass = objc_get_class("Test");
  METHOD meth;

  cc[0] = [Test new];
  CHECK_IF (bb[3] == 3);
  [*c test2: b with: bb + 4];
  CHECK_IF (bb[3] == 4);
  bb[3] = 0;
  [*c test2: bb with: bb + 5];
  CHECK_IF (bb[3] == 5);

  meth = class_get_instance_method(testClass, @selector(test2:with:));
  offs1 = offs2 = offs3 = offs4 = offs5 = offs6 = -1;
  sscanf(method_get_types(meth), "v%d@%d:%d[%di]%d^i%d", &offs1, &offs2, &offs3,
      &offs4, &offs5, &offs6);
  CHECK_IF (!offs2 && offs4 == 5 && offs3 > 0);
  CHECK_IF (offs5 == 2 * offs3 && offs6 == 3 * offs3 && offs1 == 4 * offs3);
  
  meth = class_get_instance_method(testClass, @selector(test3:));
  offs1 = offs2 = offs3 = offs4 = offs5 = offs6 = -1;
  sscanf(method_get_types(meth), "v%d@%d:%d[%d[%d{Test=#f}]]%d", &offs1, &offs2, &offs3,
      &offs4, &offs5, &offs6);
  CHECK_IF (!offs2 && offs4 == 3 && offs5 == 4 && offs3 > 0);
  CHECK_IF (offs6 == 2 * offs3 && offs1 == 3 * offs3);
  
  return 0;
}
