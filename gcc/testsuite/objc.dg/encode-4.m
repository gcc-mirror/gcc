/* Encoding tests for ObjC class layouts.  */
/* Contributed by Ziemowit Laski <zlaski@apple.com>.  */
/* { dg-options "-lobjc" } */
/* { dg-do run } */

#include <objc/Object.h>
#ifdef __NEXT_RUNTIME__
#include <objc/objc-class.h>
#define OBJC_GETCLASS objc_getClass
#else
#include <objc/objc-api.h>
#define OBJC_GETCLASS objc_get_class
#endif

extern void abort(void);
extern int strcmp(const char *s1, const char *s2);
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

struct objc_ivar *ivar;

static void check_ivar(const char *name, const char *type) {
  CHECK_IF(!strcmp(ivar->ivar_name, name));
  CHECK_IF(!strcmp(ivar->ivar_type, type));
  ivar++;
}

int main(void) {
  ivar = ((Class)OBJC_GETCLASS("Int1"))->ivars->ivar_list;
  check_ivar("a", "c");
  check_ivar("b", "c");
  check_ivar("int2", "@\"Int2\"");
  check_ivar("nested", 
    "{Nested=\"a\"f\"b\"f\"next\"@\"Int1\"\"innermost\"{Innermost=\"a\"C\"b\"C\"encl\"^{Nested}}}");
    
  ivar = ((Class)OBJC_GETCLASS("Int2"))->ivars->ivar_list;
  check_ivar("innermost", "^{Innermost=CC^{Nested}}");
  check_ivar("base", "@\"Int1\"");
  
  return 0;
}
