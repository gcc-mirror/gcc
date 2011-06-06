/* Sanity check for GNU-runtime regardless of runtime used on target system.  */

/* { dg-do run } */
/* { dg-skip-if "" { *-*-* } { "-fnext-runtime" } { "" } } */

#include "../objc-obj-c++-shared/TestsuiteObject.m"
#include <string.h>
#include <stdlib.h>

int main(int argc, void **args)
{
  [TestsuiteObject new];
  return 0;
}
