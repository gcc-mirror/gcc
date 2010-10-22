/* Based on a test case contributed by Nicola Pero.  */

/* { dg-do run } */
/* { dg-options "-mno-constant-cfstrings" { target *-*-darwin* } } */
/* { dg-xfail-run-if "Needs OBJC2 ABI" { *-*-darwin* && { lp64 && { ! objc2 } } } { "-fnext-runtime" } { "" } } */
/* { dg-additional-sources "../../../objc-obj-c++-shared/Object1.m" } */

#include "../../../objc-obj-c++-shared/Object1.h"
#import "../../../objc-obj-c++-shared/next-mapping.h"
#include <string.h>
#include <stdlib.h>

#ifndef __NEXT_RUNTIME__
#include <objc/NXConstStr.h>
#endif

#define STRING "this is a string"

int main (int argc, void **args)
{
  if (strcmp ([@STRING cString], STRING))
    abort ();
  return 0;
}
