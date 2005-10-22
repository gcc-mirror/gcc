/* { dg-do run } */

/* This tests that exceptions work.  It used to fail because
   objc_msgSend was marked with DECL_NOTHROW. 
   If you include objc/Object.h, the problem goes away, because
   that file includes objc/objc-runtime.h which explicitly prototypes
   objc_msgSend without 'nothrow'.  */

#include <stdio.h>
#include <stdlib.h>


@interface Object {
  Class isa;  
}
+ alloc;
- init;
@end

// ObjectiveC class header
@interface ObjCclass : Object {
}
-(void)method1;
-(void)method2;
@end

// C++ class header
class CPPclass {
public:
	void function1();
};


// Main
int main(int argc, char *argv[])
{
	ObjCclass * foo = [[ObjCclass alloc] init];
	[foo method1];
	exit (0);
}


// ObjectiveC implementation
@implementation ObjCclass

-(void) method1
{
	try {
		[self method2];
	}
	catch(...) {
		return;
	}
}

-(void) method2
{
	CPPclass foo;
	foo.function1();
}

@end


// C++ implementation
void CPPclass::function1()
{
	throw (1);
	/* Shouldn't be here because we threw.  */
	abort ();
}
