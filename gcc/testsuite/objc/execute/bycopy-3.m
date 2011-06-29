/*
 * Contributed by Nicola Pero <nicola@brainstorm.co.uk>
 * Wed Feb 28 12:27:03 CET 2001
 */

/*
 * This test contains some no-op code which is needed to keep it
 * compile on broken gcc 3.x.  Anyway, the no-op code does not
 * interfere with what we are testing, which is that the `bycopy'
 * keyword generates the _F_BYCOPY qualifier for the return type.  */

extern int printf (const char *, ...);

#include <objc/objc.h>
#include "../../objc-obj-c++-shared/runtime.h"
#include "../../objc-obj-c++-shared/TestsuiteObject.m"

@protocol MyProtocol
+ (bycopy id<MyProtocol>) bycopyMethod;
@end

/* This no-op class to keep it compile under broken gcc 3.x */
@interface MyObject : TestsuiteObject <MyProtocol> 
@end

@implementation MyObject
+ (bycopy id<MyProtocol>) bycopyMethod
{
  return [MyObject alloc];
}
@end

/* The following header, together with the implementation included below,
   emulate functionality provided by the GNU runtime but not available from
   the NeXT runtime.  */
#include "../../objc-obj-c++-shared/objc-test-suite-next-encode-assist.h"

int main (void)
{
  struct objc_method_description method;
  const char *method_types;
  unsigned qualifiers;
  Protocol *protocol;
  /* This no-op command is needed to keep the test compile on broken
     gcc 3.x */
  MyObject *object = [MyObject bycopyMethod];

  /* Get the protocol object */
  protocol = @protocol (MyProtocol);

  /* Ask to the protocol for the description of the method bycopyMethod */
  method = protocol_getMethodDescription (protocol, @selector (bycopyMethod),
					  YES, NO);

  /* Get the method types for the method - which encode return type,
     arguments etc. */
  method_types = method.types;

  if (method_types == NULL)
    {
      printf ("Could not find method bycopyMethod in protocol!\n");
      return 1;
    }

  /* Get the qualifiers for the return type */
  qualifiers = objc_get_type_qualifiers (method_types);

  /* If _F_BYCOPY is not there, the compiler is broken */
  if (! (qualifiers & _F_BYCOPY))
    {
      printf ("Failed - selector does not contain _F_BYCOPY qualifier!\n");
      return 1;
    }

  /* Else, happy end */
  return 0;
}

#ifdef __NEXT_RUNTIME__
unsigned
objc_get_type_qualifiers (const char *type)
{
  unsigned res = 0;
  BOOL flag = YES;

  while (flag)
    switch (*type++)
      {
      case _C_CONST:	res |= _F_CONST; break;
      case _C_IN:	res |= _F_IN; break;
      case _C_INOUT:	res |= _F_INOUT; break;
      case _C_OUT:	res |= _F_OUT; break;
      case _C_BYCOPY:	res |= _F_BYCOPY; break;
      case _C_BYREF:  res |= _F_BYREF; break;
      case _C_ONEWAY:	res |= _F_ONEWAY; break;
      case _C_GCINVISIBLE: res |= _F_GCINVISIBLE; break;
      default: flag = NO;
    }

  return res;
}
#endif
