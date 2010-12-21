/* Test a little inefficiency that was fixed in libobjc when dealing
   with selectors (PR libobjc/45953).  */

/* { dg-do run } */
/* { dg-skip-if "" { *-*-* } { "-fnext-runtime" } { "" } } */

/* To get the modern GNU Objective-C Runtime API, you include
   objc/runtime.h.  */
#include <objc/runtime.h>
#include <stdlib.h>

/* Test that registering a new selector, with the same name but a
   different type than the previous one, does not change the original
   name string.  It is actually fine to change it (there is no
   guarantee that it won't change), except for runtime performance /
   memory consumption, since changing it means that the runtime is
   doing an unneeded objc_malloc()/strcpy(), which is inefficient.  */

int main (void)
{
  SEL selector_1;
  SEL selector_2;
  const char *name_1;
  const char *name_2;

  /* These method type strings may well be invalid.  Please don't use
     them as examples.  They are irrelevant for this test; any string
     will do.  */
  selector_1 = sel_registerTypedName ("method", "v@:");
  name_1 = sel_getName (selector_1);

  selector_2 = sel_registerTypedName ("method", "i@:");
  name_2 = sel_getName (selector_1);

  if (name_1 != name_2)
    abort ();

  return 0;
}
