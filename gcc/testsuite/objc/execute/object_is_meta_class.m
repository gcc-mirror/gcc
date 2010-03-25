/* Contributed by Nicola Pero - Tue Jul  3 10:55:21 BST 2001 */
#import "../../objc-obj-c++-shared/Object1.h"
#import "../../objc-obj-c++-shared/next-mapping.h"
#include <objc/objc-api.h>

/* This test demonstrate a failure in object_is_meta_class which was fixed */

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
  /* The following one is used in the runtime to mark meta classes */
  info = 0x2L;
  return self;
}
@end

int main (void)
{
  /* Create an object of our EvilClass */
  EvilClass *evilObject = [EvilClass new];
  
  /* Now check that the object is not a meta class object */
  if (object_is_meta_class (evilObject))
    {
      printf ("object_is_meta_class failed\n");
      abort ();
    }

  return 0;
}

