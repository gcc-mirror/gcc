/* Test if ObjC constant string layout is checked properly, regardless of how
   constant string classes get derived.  */
/* Contributed by Ziemowit Laski <zlaski@apple.com>  */

/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-fgnu-runtime" } { "" } } */
/* { dg-options "-mno-constant-cfstrings" { target *-*-darwin* } } */

#include "../../../objc-obj-c++-shared/Object1.h"

@interface NSString: Object
@end

@interface NSSimpleCString : NSString {
@protected
    char *bytes;
    unsigned int numBytes;
}
@end
    
@interface NSConstantString : NSSimpleCString
@end

#ifdef NEXT_OBJC_USE_NEW_INTERFACE
Class _NSConstantStringClassReference;
#else
extern struct objc_class _NSConstantStringClassReference;
#endif

const NSConstantString *appKey = @"MyApp";

/* { dg-final { scan-assembler ".section __OBJC, __cstring_object" { target { *-*-darwin* && { ! lp64 } } } } } */
/* { dg-final { scan-assembler ".section __DATA, __objc_stringobj" { target { *-*-darwin* && { lp64 } } } } } */
/* { dg-final { scan-assembler ".long\t__NSConstantStringClassReference\n\t.long\t.*\n\t.long\t5\n\t.data" { target { *-*-darwin* && { ! lp64 } } } } } */
/* { dg-final { scan-assembler ".quad\t_OBJC_CLASS_._NSConstantString\n\t.quad\t.*\n\t.long\t5\n\t.space" { target { *-*-darwin* && { lp64 } } } } } */
