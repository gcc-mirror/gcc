/* Test if ObjC constant string layout is checked properly, regardless of how
   constant string classes get derived.  */
/* Contributed by Ziemowit Laski <zlaski@apple.com>  */

/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-fgnu-runtime" } { "" } } */
/* { dg-options "-fconstant-string-class=XStr" } */
/* { dg-options "-mno-constant-cfstrings -fconstant-string-class=XStr" { target *-*-darwin* } } */

#include <objc/Object.h>
#include "../../../objc-obj-c++-shared/runtime.h" /* For NEXT_OBJC_USE_NEW_INTERFACE.  */

@interface XString: Object {
@protected
    char *bytes;
}
@end

@interface XStr : XString {
@public
    unsigned int len;
}
@end

#ifdef NEXT_OBJC_USE_NEW_INTERFACE
extern Class _XStrClassReference;
#else
extern struct objc_class _XStrClassReference;
#endif

const XStr *appKey = @"MyApp";

/* { dg-final { scan-assembler ".section __OBJC, __cstring_object" { target { *-*-darwin* && { ! lp64 } } } } } */
/* { dg-final { scan-assembler ".section __DATA, __objc_stringobj" { target { *-*-darwin* && { lp64 } } } } } */
/* { dg-final { scan-assembler ".long\t__XStrClassReference\n\t.long\t.*\n\t.long\t5\n\t.data"  { target { *-*-darwin* && { ! lp64 } } } } } */
/* { dg-final { scan-assembler ".quad\t_OBJC_CLASS_._XStr\n\t.quad\t.*\n\t.long\t5\n\t.space" { target { *-*-darwin* && { lp64 } } } } } */
