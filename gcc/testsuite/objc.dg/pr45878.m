/* { dg-do compile } */
/* { dg-options "-O2 -fexceptions" } */

typedef struct objc_object { Class class_pointer; } *id;
typedef unsigned char  BOOL;

@interface Object
{
  Class isa;
}
- (BOOL)isEqual:anObject;
@end

@interface NXConstantString: Object
{
  char *c_string;
  unsigned int len;
}
@end

void function (void)
{
  if ([@"strings" isEqual: (id)0])
    {
      ;
    }
}

