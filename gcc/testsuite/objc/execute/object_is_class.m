/* Contributed by Nicola Pero - Tue Jul  3 10:55:21 BST 2001 */
#ifdef __NEXT_RUNTIME__
#  include "../../objc-obj-c++-shared/next-mapping.h"
#else
#  include <objc/objc-api.h>
#endif
#include "../../objc-obj-c++-shared/Object1.h"

/* This test demonstrate a failure in object_is_class which was fixed */

/* Create a class whose instance variables mirror the struct used for
   Class structures in the runtime ... yes we're feeling evil today */
@interface EvilClass : Object
{
  Class super_class;
  const char* name;
  long version;
  unsigned long info;    
}
@end

@implementation EvilClass
- (id) init
{
  self = [super init];
  /* The following one is used in the runtime to mark classes */
  info = 0x1L;
  return self;
}
@end

int main (void)
{
  /* Create an object of our EvilClass */
  EvilClass *evilObject = [EvilClass new];
  
  /* Now check that the object is not a class object */
  if (object_is_class (evilObject))
    {
      printf ("object_is_class failed\n");
      abort ();
    }

  return 0;
}
#include "../../objc-obj-c++-shared/Object1-implementation.h"
