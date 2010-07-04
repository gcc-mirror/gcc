/* Encoding tests for ObjC class layouts.  */
/* Contributed by Ziemowit Laski <zlaski@apple.com>.  */
/* { dg-options "" } */
/* { dg-do run } */
/* { dg-xfail-run-if "Needs OBJC2 ABI" { *-*-darwin* && { lp64 && { ! objc2 } } } { "-fnext-runtime" } { "" } } */
#include "../objc-obj-c++-shared/Object1.h"
#include "../objc-obj-c++-shared/next-mapping.h"
#ifndef __NEXT_RUNTIME__
#include <objc/objc-api.h>
#endif

#include <stdlib.h>
#include <string.h>

#define CHECK_IF(expr) if(!(expr)) abort()

@class Int1, Int2;
struct Nested;

struct Innermost {
  unsigned char a, b;
  struct Nested *encl;
};

struct Nested {
  float a, b;
  Int1 *next;
  struct Innermost innermost;
};

@interface Int1: Object {
  signed char a, b;
  Int2 *int2;
  struct Nested nested;
}
@end

@interface Int2: Int1 {
  struct Innermost *innermost;
  Int1 *base;
}
@end

@implementation Int1
@end

@implementation Int2
@end

#ifdef NEXT_OBJC_USE_NEW_INTERFACE
Ivar *ivar;
#else
struct objc_ivar *ivar;
#endif

static void check_ivar(const char *name, const char *type) {
#ifdef NEXT_OBJC_USE_NEW_INTERFACE
  CHECK_IF(!strcmp(ivar_getName(*ivar), name));
  CHECK_IF(!strcmp(ivar_getTypeEncoding(*ivar), type));
#else
  CHECK_IF(!strcmp(ivar->ivar_name, name));
  CHECK_IF(!strcmp(ivar->ivar_type, type));
#endif
  ivar++;
}

int main(void) {
#ifdef NEXT_OBJC_USE_NEW_INTERFACE
  ivar = class_copyIvarList ((Class)objc_get_class("Int1"), NULL);
#else
  ivar = ((Class)objc_get_class("Int1"))->ivars->ivar_list;
#endif
  check_ivar("a", "c");
  check_ivar("b", "c");
  check_ivar("int2", "@\"Int2\"");
  check_ivar("nested", 
    "{Nested=\"a\"f\"b\"f\"next\"@\"Int1\"\"innermost\"{Innermost=\"a\"C\"b\"C\"encl\"^{Nested}}}");
    
#ifdef NEXT_OBJC_USE_NEW_INTERFACE
  ivar = class_copyIvarList ((Class)objc_get_class("Int2"), NULL);
#else
  ivar = ((Class)objc_get_class("Int2"))->ivars->ivar_list;
#endif
  check_ivar("innermost", "^{Innermost=CC^{Nested}}");
  check_ivar("base", "@\"Int1\"");
  
  return 0;
}
#include "../objc-obj-c++-shared/Object1-implementation.h"
