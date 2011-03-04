/* Test the Modern GNU Objective-C Runtime API.

  This is test 'property', covering all functions starting with 'property'.  */

/* { dg-do run } */
/* { dg-skip-if "No API#2 pre-Darwin9" { *-*-darwin[5-8]* } { "-fnext-runtime" } { "" } } */

/* To get the modern GNU Objective-C Runtime API, you include
   objc/runtime.h.  */
#include <objc/runtime.h>
#include <stdlib.h>
#include <iostream>
#include <cstring>

@interface MyRootClass
{ Class isa; }
+ alloc;
- init;
+ initialize;
@end

@implementation MyRootClass
+ alloc { return class_createInstance (self, 0); }
- init  { return self; }
+ initialize { return self; }
@end

@interface MySubClass : MyRootClass
{
  id propertyA;
  id propertyB;
}
@property (assign, getter=getP, setter=setP:) id propertyA;
@property (assign, nonatomic) id propertyB;
@end

@implementation MySubClass
@synthesize propertyA;
@synthesize propertyB;
@end


int main ()
{
  /* Functions are tested in alphabetical order.  */

  std::cout << "Testing property_getAttributes () ...\n";
  {
    /* The Apple/NeXT runtime seems to crash on the following.  */
#ifdef __GNU_LIBOBJC__
    if (property_getAttributes (NULL) != NULL)
      abort ();
#endif

    /* The GNU runtime doesn't support looking up properties at
       runtime yet.  */
#ifdef __OBJC2__
    {
      objc_property_t property;
      
      property = class_getProperty (objc_getClass ("MySubClass"), "propertyA");
      if (std::strcmp (property_getAttributes (property),
		  "T@,GgetP,SsetP:,VpropertyA") != 0)
	abort ();

      property = class_getProperty (objc_getClass ("MySubClass"), "propertyB");
      if (std::strcmp (property_getAttributes (property),
		  "T@,N,VpropertyB") != 0)
	abort ();
    }
#endif    
  }

  std::cout << "Testing property_getName () ...\n";
  {
    /* The Apple/NeXT runtime seems to crash on the following.  */
#ifdef __GNU_LIBOBJC__

    if (property_getName (NULL) != NULL)
      abort ();
#endif

    /* The GNU runtime doesn't support looking up properties at
       runtime yet.  */
#ifdef __OBJC2__
    {
      objc_property_t property;
      
      property = class_getProperty (objc_getClass ("MySubClass"), "propertyA");
      if (std::strcmp (property_getName (property), "propertyA") != 0)
	abort ();

      property = class_getProperty (objc_getClass ("MySubClass"), "propertyB");
      if (std::strcmp (property_getName (property), "propertyB") != 0)
	abort ();
    }
#endif
  }

  return (0);
}
