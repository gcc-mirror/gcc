/* This file tests that things are encoded using the gcc-3.3 ABI which is only
   used by the NeXT runtime.  */
/* { dg-do run { target *-*-darwin* } } */
/* { dg-require-effective-target ilp32 } */
/* { dg-skip-if "" { *-*-* } { "-fgnu-runtime" } { "" } } */

#include <stdio.h>
#include <string.h>
#include "../objc-obj-c++-shared/runtime.h"

extern void abort();

@protocol CommonProtocol

-(oneway void)methodCall_On:(in bycopy id)someValue_On;
-(oneway void)methodCall_nO:(bycopy in id)someValue_nO;

-(oneway void)methodCall_Oo:(out bycopy id)someValue_Oo;
-(oneway void)methodCall_oO:(bycopy out id)someValue_oO;

-(oneway void)methodCall_rn:(in const id)someValue_rn;

-(oneway void)methodCall_oOn:(in bycopy out id)someValue_oOn;

@end

@interface ObjCClass <CommonProtocol>
{

}

@end

@implementation ObjCClass
-(oneway void)methodCall_On:(in bycopy id)someValue_On { }
-(oneway void)methodCall_nO:(bycopy in id)someValue_nO { }

-(oneway void)methodCall_Oo:(out bycopy id)someValue_Oo { }
-(oneway void)methodCall_oO:(bycopy out id)someValue_oO { }

-(oneway void)methodCall_rn:(in const id)someValue_rn { }
-(oneway void)methodCall_oOn:(in bycopy out id)someValue_oOn { }
@end

Protocol *proto = @protocol(CommonProtocol);
struct objc_method_description *meth;
struct objc_method_description meth_object;

int main()
{
        meth_object = protocol_getMethodDescription (proto, @selector(methodCall_On:), YES, YES);
        meth = &meth_object;
	if (strcmp (meth->types, "Vv12@0:4On@8"))
	  abort();
        meth_object = protocol_getMethodDescription (proto, @selector(methodCall_nO:), YES, YES);
        meth = &meth_object;
	if (strcmp (meth->types, "Vv12@0:4nO@8"))
	  abort();
        meth_object = protocol_getMethodDescription (proto, @selector(methodCall_Oo:), YES, YES);
        meth = &meth_object;
	if (strcmp (meth->types, "Vv12@0:4Oo@8"))
	  abort();
        meth_object = protocol_getMethodDescription (proto, @selector(methodCall_oO:), YES, YES);
        meth = &meth_object;
	if (strcmp (meth->types, "Vv12@0:4oO@8"))
	  abort();
        meth_object = protocol_getMethodDescription (proto, @selector(methodCall_rn:), YES, YES);
        meth = &meth_object;
	if (strcmp (meth->types, "Vv12@0:4rn@8"))
	  abort();
        meth_object = protocol_getMethodDescription (proto, @selector(methodCall_oOn:), YES, YES);
        meth = &meth_object;
	if (strcmp (meth->types, "Vv12@0:4oOn@8"))
	  abort();
	return 0;
}
