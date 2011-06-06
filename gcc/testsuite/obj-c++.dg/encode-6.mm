/* Encoding tests for ObjC class layouts.  */
/* Contributed by Ziemowit Laski <zlaski@apple.com>.  */
/* { dg-options "" } */
/* { dg-do run } */
/* { dg-xfail-run-if "Needs OBJC2 ABI" { *-*-darwin* && { lp64 && { ! objc2 } } } { "-fnext-runtime" } { "" } } */
#include "../objc-obj-c++-shared/TestsuiteObject.m"
#include "../objc-obj-c++-shared/runtime.h"

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

@interface Int1: TestsuiteObject {
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

#if defined(__NEXT_RUNTIME__) && !defined(NEXT_OBJC_USE_NEW_INTERFACE)
struct objc_ivar *ivar;
#else
Ivar *ivar;
#endif

static void check_ivar(const char *name, const char *type) {
#if defined(__NEXT_RUNTIME__) && !defined(NEXT_OBJC_USE_NEW_INTERFACE)
  CHECK_IF(!strcmp(ivar->ivar_name, name));
  CHECK_IF(!strcmp(ivar->ivar_type, type));
#else
  CHECK_IF(!strcmp(ivar_getName(*ivar), name));
  CHECK_IF(!strcmp(ivar_getTypeEncoding(*ivar), type));
#endif
  ivar++;
}

int main(void) {
#if defined(__NEXT_RUNTIME__) && !defined(NEXT_OBJC_USE_NEW_INTERFACE)
  ivar = ((Class)objc_getClass("Int1"))->ivars->ivar_list;
#else
  ivar = class_copyIvarList ((Class)objc_getClass("Int1"), NULL);
#endif
  check_ivar("a", "c");
  check_ivar("b", "c");
  check_ivar("int2", "@\"Int2\"");
  check_ivar("nested", 
    "{Nested=\"a\"f\"b\"f\"next\"@\"Int1\"\"innermost\"{Innermost=\"a\"C\"b\"C\"encl\"^{Nested}}}");
    
#if defined(__NEXT_RUNTIME__) && !defined(NEXT_OBJC_USE_NEW_INTERFACE)
  ivar = ((Class)objc_getClass("Int2"))->ivars->ivar_list;
#else
  ivar = class_copyIvarList ((Class)objc_getClass("Int2"), NULL);
#endif
  check_ivar("innermost", "^{Innermost=CC^{Nested}}");
  check_ivar("base", "@\"Int1\"");
  
  return 0;
}

