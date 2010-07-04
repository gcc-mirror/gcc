/*
 * Contributed by Nicola Pero <nicola@brainstorm.co.uk>
 * Wed Feb 28 12:27:03 CET 2001
 */

/*
 * This test contains some no-op code which is needed to keep it
 * compile on broken gcc 3.x.  Anyway, the no-op code does not
 * interfere with what we are testing, which is that the `bycopy'
 * keyword generates the _F_BYCOPY qualifier for the return type.  */

#include "../../objc-obj-c++-shared/next-mapping.h"
#include "../../objc-obj-c++-shared/Protocol1.h"

#ifndef __NEXT_RUNTIME__
#include <objc/encoding.h>
#endif

@protocol MyProtocol
+ (bycopy id<MyProtocol>) bycopyMethod;
@end

/* This no-op class to keep it compile under broken gcc 3.x */
@interface MyObject : Object <MyProtocol> 
@end

@implementation MyObject
+ (bycopy id<MyProtocol>) bycopyMethod
{
  return [MyObject alloc];
}
@end

int main (void)
{
  struct objc_method_description *method;
  const char *method_types;
  unsigned qualifiers;
  Protocol *protocol;
  /* This no-op command is needed to keep the test compile on broken
     gcc 3.x */
  MyObject *object = [MyObject bycopyMethod];

  /* Get the protocol object */
  protocol = @protocol (MyProtocol);

  /* Ask to the protocol for the description of the method bycopyMethod */
  method = [protocol descriptionForClassMethod: @selector (bycopyMethod)];
  if (method == NULL)
    {
      printf ("Could not find method bycopyMethod in protocol!\n");
      exit (1);
    }

  /* Get the method types for the method - which encode return type,
     arguments etc. */
  method_types = method->types;

  /* Get the qualifiers for the return type */
  qualifiers = objc_get_type_qualifiers (method_types);

  /* If _F_BYCOPY is not there, the compiler is broken */
  if (! (qualifiers & _F_BYCOPY))
    {
      printf ("Failed - selector does not contain _F_BYCOPY qualifier!\n");
      exit (1);
    }

  /* Else, happy end */
  return 0;
}
