/* Make sure that @synchronized parses.  */
/* { dg-options "-fobjc-exceptions" } */
/* { dg-do compile } */

#include "../objc-obj-c++-shared/Object1.h"

void foo(id sem)
{
  @synchronized (sem) { 
    return;
  }
}
