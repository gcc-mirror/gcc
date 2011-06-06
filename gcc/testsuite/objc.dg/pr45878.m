/* { dg-do compile } */
/* { dg-options "-O2 -fexceptions" } */

typedef struct objc_object { Class class_pointer; } *id;
typedef unsigned char  BOOL;

@interface Object
{
  Class isa;
}
@end

@interface Object (Test)
- (BOOL)testIsEqual:anObject;
@end

@implementation Object (Test)
- (BOOL)testIsEqual:anObject
{
  return self == anObject;
}
@end

#ifdef __NEXT_RUNTIME__
@interface NSConstantString: Object
{
  char *c_string;
  unsigned int len;
}
@end
extern void *_NSConstantStringClassReference;
#else
@interface NXConstantString: Object
{
  char *c_string;
  unsigned int len;
}
@end
#endif

void function (void)
{
  if ([@"strings" testIsEqual: (id)0])
    {
      ;
    }
}

