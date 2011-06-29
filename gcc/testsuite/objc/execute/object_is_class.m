/* Contributed by Nicola Pero - Tue Jul  3 10:55:21 BST 2001 */

#include "../../objc-obj-c++-shared/runtime.h"
#include "../../objc-obj-c++-shared/TestsuiteObject.m"

/* This test demonstrate a failure in object_is_class which was fixed */

/* Create a class whose instance variables mirror the struct used for
   Class structures in the runtime ... yes we're feeling evil today */
@interface EvilClass : TestsuiteObject
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
  if (class_isMetaClass (object_getClass (evilObject)))
    {
      printf ("object_is_class failed\n");
      abort ();
    }

  return 0;
}
