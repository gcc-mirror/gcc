/* When assigning function pointers, allow for covariant return types
   and contravariant argument types.  */
/* { dg-do compile } */
#include "../objc-obj-c++-shared/TestsuiteObject.h"

@class Derived;

TestsuiteObject *ExternFunc (TestsuiteObject *filePath, TestsuiteObject *key);
typedef id FuncSignature (TestsuiteObject *arg1, Derived *arg2);

@interface Derived: TestsuiteObject
+ (void)registerFunc:(FuncSignature *)function;
@end

void foo(void)
{
  [Derived registerFunc: ExternFunc];
}

