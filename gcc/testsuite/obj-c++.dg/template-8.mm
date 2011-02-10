/* Test that all pending instantiations have taken place before meta-data
   generation. */	
/* Author: Fariborz Jahanian <fjahanian@apple.com> */
/* Adapted by Nicola Pero <nicola.pero@meta-innovation.com> */
/* { dg-do run } */
/* { dg-skip-if "No API#2 pre-Darwin9" { *-*-darwin[5-8]* } { "-fnext-runtime" } { "" } } */
/* { dg-xfail-run-if "Needs OBJC2 ABI" { *-*-darwin* && { lp64 && { ! objc2 } } } { "-fnext-runtime" } { "" } } */

#include <objc/objc.h>
#include <objc/runtime.h>

@interface MyRootClass
{ Class isa; }
+ (id) initialize;
+ alloc;
- init;
- doSomething;
@end

@implementation MyRootClass
+ (id) initialize { return self; }
+ alloc { return class_createInstance (self, 0); }
- init  { return self; }
- doSomething { return self; }
@end

class Base
{
public:
	Base() { }
	virtual ~Base() { }
	
	void destroy() { delete this; }
};

template<class T>
class Subclass : public T
{
public:
	Subclass() { }
	
	virtual ~Subclass()
	{
		[[[MyRootClass alloc] init] doSomething];
	}
};

int main(int argc, const char * argv[])
{
    Subclass<Base>* theSubclass = new Subclass<Base>();
    theSubclass->destroy();
    return 0;
}
