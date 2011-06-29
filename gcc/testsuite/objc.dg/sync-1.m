/* Make sure that @synchronized parses.  */
/* { dg-options "-fobjc-exceptions" } */
/* { dg-do compile } */

#include <objc/objc.h>

void foo(id sem)
{
  @synchronized (sem) { 
    return;
  }
}
