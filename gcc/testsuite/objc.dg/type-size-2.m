/* Make sure that array arguments to methods are given the size of pointers.  */
/* As in the case of ivars, arrays without size (e.g., 'int []') are
   encoded as pointers.  */
/* Contributed by Ziemowit Laski <zlaski@apple.com>.  */
/* { dg-do run } */
/* { dg-xfail-run-if "Needs OBJC2 ABI" { *-*-darwin* && { lp64 && { ! objc2 } } } { "-fnext-runtime" } { "" } } */
/* { dg-additional-sources "../objc-obj-c++-shared/Object1.m" } */

#include "../objc-obj-c++-shared/Object1.h"
#include "../objc-obj-c++-shared/next-mapping.h"
#include <stdio.h>
#include <stdlib.h>
#ifdef __NEXT_RUNTIME__
#include <objc/objc-runtime.h>
#define METHOD Method
#else
#include <objc/objc-api.h>
#define METHOD Method_t
#define method_get_types(M) (M)->method_types
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
METHOD meth ;

unsigned totsize, offs0, offs1, offs2, offs3, offs4, offs5, offs6, offs7;

static void scan_initial(const char *pattern) {
  totsize = offs0 = offs1 = offs2 = offs3 = offs4 = offs5 = offs6 = offs7 = (unsigned)-1;
  sscanf(method_get_types(meth), pattern, &totsize, &offs0, &offs1, &offs2, &offs3,
      &offs4, &offs5, &offs6, &offs7);
  CHECK_IF(!offs0 && offs1 == sizeof(id) && offs2 == offs1 + sizeof(SEL) && totsize >= offs2);
}

int main(void) {
  cls = objc_get_class("ArrayTest");

  meth = class_get_instance_method(cls, @selector(str:with:and:));

  /* Here we have the complication that 'enum Enum' could be encoded
     as 'i' on __NEXT_RUNTIME_, and (most likely) as 'I' on the GNU
     runtime.  So we get the @encode(enum Enum), then put it into the
     string in place of the traditional 'i'.
  */
  /* scan_initial("r*%u@%u:%u*%u*%u[4i]%u"); */
  {
    char pattern[1024];

    sprintf (pattern, "r*%%u@%%u:%%u*%%u*%%u[4%s]%%u", @encode(enum Enum));
    scan_initial(pattern);
  }

  CHECK_IF(offs3 == offs2 + sizeof(signed char *) && offs4 == offs3 + sizeof(unsigned char *));
  CHECK_IF(totsize == offs4 + sizeof(enum Enum *));
  meth = class_get_instance_method(cls, @selector(meth1:with:with:));
  scan_initial("i%u@%u:%u^i%u[0i]%u[2i]%u");
  CHECK_IF(offs3 == offs2 + sizeof(int *) && offs4 == offs3 + sizeof(int *));
  CHECK_IF(totsize == offs4 + sizeof(int *));                                           
  return 0;
}
