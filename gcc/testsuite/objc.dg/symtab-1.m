/* Check if the objc_symtab descriptor is being laid out correctly.  */
/* Contributed by Ziemowit Laski <zlaski@apple.com>.  */
/* { dg-options "-fnext-runtime" } */
/* { dg-do compile { target *-*-darwin* } } */

#include <objc/Object.h>

@interface Base: Object 
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

/* { dg-final { scan-assembler "L_OBJC_SYMBOLS.*:\n\t.long\t0\n\t.long\t0\n\t.short\t2\n\t.short\t0\n\t.long\tL_OBJC_CLASS_Derived.*\n\t.long\tL_OBJC_CLASS_Base.*\n" } } */
