/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, December 2010.  */

#include <stdio.h>
#include <stdlib.h>
#include <objc/objc.h>

#include "load-category-2.h"

/* Compile the categories in random order to prevent the runtime from
   sending +load in the correct order just because the classes happen
   to have been compiled in that order.  */
@implementation TestClass2 (Category)
+ load
{
  printf ("[TestClass2(Category) +load]\n");

  /* Check that the corresponding class's +load was done.  */
  check_that_load_step_was_completed (1);

  complete_load_step (4);
}
@end

@implementation TestClass3 (Category)
+ load
{
  printf ("[TestClass3(Category) +load]\n");

  /* Check that the corresponding class's +load was done.  */
  check_that_load_step_was_completed (2);

  complete_load_step (5);
}
@end

@implementation TestClass1 (Category)
+ load
{
  printf ("[TestClass1(Category) +load]\n");

  /* Check that the corresponding class's +load was done.  */
  check_that_load_step_was_completed (0);

  complete_load_step (3);
}
@end
