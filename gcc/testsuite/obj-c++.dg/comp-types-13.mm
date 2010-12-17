/* When assigning function pointers, allow for covariant return types
   and contravariant argument types.  */
/* { dg-do compile } */
#include <objc/Object.h>

@class Derived;

Object *ExternFunc (Object *filePath, Object *key);
typedef id FuncSignature (Object *arg1, Derived *arg2);

@interface Derived: Object
+ (void)registerFunc:(FuncSignature *)function;
@end

void foo(void)
{
  [Derived registerFunc: ExternFunc];
}

