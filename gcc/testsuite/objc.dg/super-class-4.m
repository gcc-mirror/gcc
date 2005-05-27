/* Bail out gracefully if attempting to derive from a class that has only been
   forward-declared (via @class).  Conversely, @compatibility_alias declarations
   should be traversed to find the @interface.  */
/* { dg-do compile } */

#include <objc/Object.h>

@class MyWpModule;

@compatibility_alias MyObject Object;
@compatibility_alias FictitiousModule MyWpModule;

@protocol MySelTarget
- (id) meth1;
@end

@protocol Img
- (id) meth2;
@end

@interface FunnyModule: FictitiousModule <Img> /* { dg-error ".MyWpModule., superclass of .FunnyModule." } */
- (id) meth2;
@end

@interface MyProjWpModule : MyWpModule <MySelTarget, Img> /* { dg-error ".MyWpModule., superclass of .MyProjWpModule." } */ {
  id i1, i2;
}
- (id) meth1;
- (id) meth2;
@end

@interface AnotherModule: MyObject <MySelTarget>
- (id) meth1;
@end
