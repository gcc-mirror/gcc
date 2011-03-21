/* Based on a test case contributed by Nicola Pero.  */

/* { dg-do run } */
/* { dg-options "-mno-constant-cfstrings" { target *-*-darwin* } } */
/* { dg-xfail-run-if "Needs OBJC2 ABI" { *-*-darwin* && { lp64 && { ! objc2 } } } { "-fnext-runtime" } { "" } } */
/* { dg-additional-sources "../../../objc-obj-c++-shared/Object1.mm ../../../objc-obj-c++-shared/nsconstantstring-class-impl.mm" } */

#include "../../../objc-obj-c++-shared/Object1.h"
#include "../../../objc-obj-c++-shared/next-mapping.h"

#include <string.h>
#include <stdlib.h>

#ifndef __NEXT_RUNTIME__
#include <objc/NXConstStr.h>
#else
#include "../../../objc-obj-c++-shared/nsconstantstring-class.h"
#endif

int main(int argc, char **args)
{
  if (strcmp ([@"this is a string" cString], "this is a string"))
    abort ();
  return 0;
}
