/* Check if the objc_symtab descriptor is being laid out correctly.  */
/* Contributed by Ziemowit Laski <zlaski@apple.com>.  */

/* { dg-do compile { target { *-*-darwin* } } } */
/* { dg-skip-if "" { *-*-* } { "-fgnu-runtime" } { "" } } */

#include <Foundation/NSObject.h>

@interface Base: NSObject 
- (void)setValues;
@end

@interface Derived: Base
- (void)checkValues;
@end

@implementation Base
-(void)setValues { }
@end

@implementation Derived
-(void)checkValues { }
@end

/* { dg-final { scan-assembler "L_OBJC_Symbols.*:\n\t.long\t0\n\t.long\t0\n\t.word\t2\n\t.word\t0\n\t.long\tL_OBJC_Class_Derived.*\n\t.long\tL_OBJC_Class_Base.*\n" { target { *86*-*-darwin* && { ! lp64 } } } } } */
/* { dg-final { scan-assembler "L_OBJC_Symbols.*:\n\t.long\t0\n\t.long\t0\n\t.short\t2\n\t.short\t0\n\t.long\tL_OBJC_Class_Derived.*\n\t.long\tL_OBJC_Class_Base.*\n" { target { powerpc*-*-darwin* && { ! lp64 } } } } } */
/* { dg-final { scan-assembler-not "L_OBJC_Symbols" { target { *-*-darwin* && { lp64 } } } } } */
