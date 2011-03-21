/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, December 2010.  */

#include <stdlib.h>
#include <objc/objc.h>

#include "load-category-1.h"

@implementation TestClass2
+ initialize { return self; }
+ load
{
  increase_load_count ();
}
@end

@implementation TestClass1 (Category)
+ load
{
  increase_load_count ();
}
@end
