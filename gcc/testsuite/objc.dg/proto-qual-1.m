/* Check that protocol qualifiers are compiled and encoded properly.  */
/* Author: Ziemowit Laski <zlaski@apple.com>  */
/* { dg-options "-lobjc" } */
/* { dg-do run } */

#include <objc/Protocol.h>
#ifndef __NEXT_RUNTIME__
#include <objc/objc-api.h>
#endif

/* The encoded parameter sizes will be rounded up to match pointer alignment.  */
#define ROUND(s,a) (a * ((s + a - 1) / a))
#define aligned_sizeof(T) ROUND(sizeof(T),__alignof(void *))

extern int sscanf(const char *str, const char *format, ...);
extern void abort(void);
#define CHECK_IF(expr) if(!(expr)) abort()

@protocol Retain
+ (oneway void)retainArgument:(out bycopy id)arg1 with:(in signed char **)arg2;
- (bycopy) address:(byref inout id)location with:(out short unsigned **)arg2;
@end

@interface Foo <Retain>
+ (oneway void)retainArgument:(out bycopy id)arg with:(in signed char **)arg2;
@end

@implementation Foo
+ (oneway void)retainArgument:(out bycopy id)arg1 with:(in signed char **)arg2 { }
- (bycopy) address:(byref inout id)location with:(out short unsigned **)arg2 { return nil; }
@end

Protocol *proto = @protocol(Retain);
struct objc_method_description *meth;
unsigned totsize, offs0, offs1, offs2, offs3, offs4, offs5, offs6, offs7;

static void scan_initial(const char *pattern) {
  totsize = offs0 = offs1 = offs2 = offs3 = offs4 = offs5 = offs6 = offs7 = (unsigned)-1;
  sscanf(meth->types, pattern, &totsize, &offs0, &offs1, &offs2, &offs3,
      &offs4, &offs5, &offs6, &offs7);
  CHECK_IF(!offs0 && offs1 == aligned_sizeof(id) && offs2 == offs1 + aligned_sizeof(SEL) && totsize >= offs2);
}

int main(void) {
  meth = [proto descriptionForInstanceMethod: @selector(address:with:)];
  scan_initial("O@%u@%u:%uNR@%uo^^S%u");
  CHECK_IF(offs3 == offs2 + aligned_sizeof(id) && totsize == offs3 + aligned_sizeof(unsigned));
  meth = [proto descriptionForClassMethod: @selector(retainArgument:with:)];
  scan_initial("Vv%u@%u:%uOo@%un^*%u");
  CHECK_IF(offs3 == offs2 + aligned_sizeof(id) && totsize == offs3 + aligned_sizeof(char **));
  return 0;
}
