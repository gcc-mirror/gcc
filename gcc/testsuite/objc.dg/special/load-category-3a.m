/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, December 2010.  */

/* This test is identical to load-category-2, but the classes and
   categories are created in inverted order in the modules, to test
   that you can load classes first, or categories first, and it all
   still works.  */

#include <stdio.h>
#include <stdlib.h>
#include <objc/objc.h>

#include "load-category-3.h"

@implementation TestClass2
+ load
{
  printf ("[TestClass2 +load]\n");
  /* Check superclasses/subclasses +load order.  */
  check_that_load_step_was_completed (0);
  check_that_load_step_was_not_completed (1);
  check_that_load_step_was_not_completed (2);

  /* Check that the corresponding category's +load was not done.  */
  check_that_load_step_was_not_completed (4);

  complete_load_step (1);
}
@end

@implementation TestClass3
+ load
{
  printf ("[TestClass3 +load]\n");

  /* Check superclasses/subclasses +load order.  */
  check_that_load_step_was_completed (0);
  check_that_load_step_was_completed (1);
  check_that_load_step_was_not_completed (2);

  /* Check that the corresponding category's +load was not done.  */
  check_that_load_step_was_not_completed (5);

  complete_load_step (2);
}
@end

@implementation TestClass1
+ initialize { return self; }
+ load
{
  printf ("[TestClass1 +load]\n");

  /* Check superclasses/subclasses +load order.  */
  check_that_load_step_was_not_completed (0);
  check_that_load_step_was_not_completed (1);
  check_that_load_step_was_not_completed (2);

  /* Check that the corresponding category's +load was not done.  */
  check_that_load_step_was_not_completed (3);

  complete_load_step (0);
}
@end


