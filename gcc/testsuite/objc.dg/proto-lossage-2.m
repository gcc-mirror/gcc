/* Don't forget to look in protocols if a class (and its superclasses) do not
   provide a suitable method.  */
/* { dg-do compile } */

#include "../objc-obj-c++-shared/TestsuiteObject.h"
#include <objc/objc.h>

@protocol Zot
-(void) zot;
@end

@interface Foo : TestsuiteObject <Zot>
@end

int foo()
{
	Foo *f=nil;
	[f zot]; /* There should be no warnings here! */
	return 0;
}

