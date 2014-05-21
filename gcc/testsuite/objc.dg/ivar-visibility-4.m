/* Test instance variable visibility.  */
/* Author: Dimitris Papavasiliou <dpapavas@gmail.com>.  */
/* { dg-do compile } */
/* { dg-additional-options "-fivar-visibility=public" } */
#include <objc/objc.h>

@interface MySuperClass
{
    int someivar;
}
@end

@implementation MySuperClass
@end


@interface MyClass : MySuperClass 
@end

@implementation MyClass
@end

@interface MyOtherClass
- (void) test: (MyClass *) object;
@end

@implementation MyOtherClass
- (void) test: (MyClass *) object
{
  int a;

  /* someivar is public so we shouldn't get any errors here. */
  
  a = object->someivar;
}
@end
