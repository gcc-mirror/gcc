/* Method encoding tests for stand-alone @protocol declarations.  */
/* Contributed by Ziemowit Laski <zlaski@apple.com>.  */
/* { dg-do run } */
/* { dg-xfail-run-if "Needs OBJC2 ABI" { *-*-darwin* && { lp64 && { ! objc2 } } } { "-fnext-runtime" } { "" } } */

#include <stdio.h>
#include <stdlib.h>

#include "../objc-obj-c++-shared/runtime.h"

#ifdef __cplusplus
#define ProtoBool bool
#else
#define ProtoBool _Bool
#endif

extern int sscanf(const char *str, const char *format, ...);
extern void abort(void);
#define CHECK_IF(expr) if(!(expr)) abort()

enum Enum {
  zero, one, two, three
};
typedef enum Enum Enum;
typedef signed char ObjCBool; /* as used by the NeXT runtime */

@protocol Proto
union __XXAngle { unsigned int alpha, beta; };
typedef struct { float x, y; union __XXAngle a; } XXPoint;
typedef struct { double width, height; } XXSize;
typedef struct _XXRect { XXPoint origin; XXSize size; struct _XXRect *next; } XXRect;
- (void) char:(signed char)c float:(float)f double:(double)d unsigned:(unsigned)u short:(short)s long:(long)l;
- (void *)setRect:(XXRect)r withBool:(ProtoBool)b withInt:(int)i;
+ (Enum *)getEnum:(XXPoint *)pt enum:(enum Enum)e bool:(ObjCBool)b;
+ (ProtoBool **)getBool:(ObjCBool **)b;
@end

Protocol *proto;
struct objc_method_description *meth;
struct objc_method_description meth_object;
unsigned totsize, offs0, offs1, offs2, offs3, offs4, offs5, offs6, offs7;

static void scan_initial(const char *pattern) {
  totsize = offs0 = offs1 = offs2 = offs3 = offs4 = offs5 = offs6 = offs7 = (unsigned)-1;
  sscanf(meth->types, pattern, &totsize, &offs0, &offs1, &offs2, &offs3,
	 &offs4, &offs5, &offs6, &offs7);
  CHECK_IF(!offs0 && offs1 == sizeof(id) && offs2 == offs1 + sizeof(SEL) && totsize >= offs2);
}

int main(void) {
  const char *string;
  proto = @protocol(Proto);
  meth_object = protocol_getMethodDescription (proto,
		   @selector(char:float:double:unsigned:short:long:), YES, YES);
  meth = &meth_object;
  if (sizeof (long) == 8)
    string = "v%u@%u:%uc%uf%ud%uI%us%uq%u";
  else
    string = "v%u@%u:%uc%uf%ud%uI%us%ul%u";
  scan_initial(string);
  CHECK_IF(offs3 == offs2 + sizeof(int) && offs4 == offs3 + sizeof(float));
  CHECK_IF(offs5 == offs4 + sizeof(double) && offs6 == offs5 + sizeof(unsigned));
  CHECK_IF(offs7 == offs6 + sizeof(int) && totsize == offs7 + sizeof(long));
  meth_object = protocol_getMethodDescription (proto,
		  @selector(setRect:withBool:withInt:), YES, YES);
  meth = &meth_object;
  scan_initial("^v%u@%u:%u{_XXRect={?=ff(__XXAngle=II)}{?=dd}^{_XXRect}}%uB%ui%u");
  CHECK_IF(offs3 == offs2 + sizeof(XXRect) && offs4 == offs3 + sizeof(int));
  CHECK_IF(totsize == offs4 + sizeof(int));
  meth_object = protocol_getMethodDescription (proto,
		  @selector(getEnum:enum:bool:), YES, NO);
  meth = &meth_object; 

  /* Here we have the complication that 'enum Enum' could be encoded
     as 'i' on __NEXT_RUNTIME_, and (most likely) as 'I' on the GNU
     runtime.  So we get the @encode(enum Enum), then put it into the
     string in place of the traditional 'i'.
  */
  /* scan_initial("^i%u@%u:%u^{?=ff(__XXAngle=II)}%ui%uc%u"); */
  {
    char pattern[1024];

    sprintf (pattern, "^%s%%u@%%u:%%u^{?=ff(__XXAngle=II)}%%u%s%%uc%%u",
	     @encode(enum Enum), @encode(enum Enum));
    scan_initial(pattern);
  }

  CHECK_IF(offs3 == offs2 + sizeof(XXPoint *) && offs4 == offs3 + sizeof(enum Enum));
  CHECK_IF(totsize == offs4 + sizeof(int));  /* 'ObjCBool' is really 'char' */
  meth_object = protocol_getMethodDescription (proto,
		  @selector(getBool:), YES, NO);
  meth = &meth_object;
  scan_initial("^^B%u@%u:%u^*%u");
  CHECK_IF(totsize == offs2 + sizeof(ObjCBool **));
  return 0;
}
