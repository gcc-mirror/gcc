/* Make sure that @synchronized parses.  */
/* { dg-options "-fnext-runtime -fobjc-exceptions" } */
/* { dg-do compile } */

#include <objc/Object.h>

void foo(id sem)
{
  @synchronized (sem) { 
    return;
  }
}
