/* Contributed by Nicola Pero on Tue Mar  6 23:05:53 CET 2001 */
#include <stdio.h>
#include <stdlib.h>
#include "../../objc-obj-c++-shared/runtime.h"

/*
 * Standard Tests For Methods of Classes and Objects - abort upon
 * failing; return normally if all is well.  
 */

/* Test that `class' has an instance method for the selector `selector' */
void test_that_class_has_instance_method (const char *class_name, 
					  SEL selector)
{
  Class class = objc_getClass (class_name);

  if (class_getInstanceMethod (class, selector) == NULL)
    {
      printf ("test_class_has_instance_method failed\n");
      abort ();
    }
}

/* Test that `class' has a class method for the selector `selector' */
void test_that_class_has_class_method (const char *class_name, 
				       SEL selector)
{
  Class class = objc_getClass (class_name);

  if (class_getClassMethod (class, selector) == NULL)
    {
      printf ("test_class_has_class_method failed\n");
      abort ();
    }
}

/* Test the accessor methods (called -state and -setState:) on the
   object `object'. */
#ifdef TYPE_OF_OBJECT_WITH_ACCESSOR_METHOD
void test_accessor_method (TYPE_OF_OBJECT_WITH_ACCESSOR_METHOD object, 
			   int initial_state,
			   int new_state_0, int expected_result_0,
			   int new_state_1, int expected_result_1)
{
  if ([object state] != initial_state)
    {
      printf ("test_accessor_method (initial state) failed\n");
      abort ();
    }
  
  [object setState: new_state_0];
  if ([object state] != expected_result_0)
    {
      printf ("test_accessor_method (new_state_0) failed\n");
      abort ();
    }  
  
  [object setState: new_state_1];
  if ([object state] != expected_result_1)
    {
      printf ("test_accessor_method (new_state_1) failed\n");
      abort ();
    }  
}
#endif /* CLASS_WITH_ACCESSOR_METHOD */


