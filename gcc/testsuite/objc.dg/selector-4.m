/* Test whether including C++ keywords such as 'and', 'or',
   'not', etc., is allowed inside ObjC selectors (as it must be).  */
/* Author: Ziemowit Laski <zlaski@apple.com>.  */

/* { dg-do compile } */

@interface Int1 
+ (int)and_eq:(int)arg1 and:(int)arg2;
- (int)or_eq:(int)arg1 or:(int)arg3;
- (int)not:(int)arg1 xor:(int)arg2;
- (void)bitand:(char)c1 bitor:(char)c2;
- (void)compl:(float)f1 xor_eq:(double)d1;
- (void)not_eq;
@end

@implementation Int1
+ (int)and_eq:(int)arg1 and:(int)arg2 { return arg1 + arg2; }
- (int)or_eq:(int)arg1 or:(int)arg3 { return arg1 + arg3; }
- (int)not:(int)arg1 xor:(int)arg2 { return arg1 + arg2; }
- (void)bitand:(char)c1 bitor:(char)c2 { }
- (void)compl:(float)f1 xor_eq:(double)d1 { }
- (void)not_eq { }
@end

/* { dg-final { scan-assembler  "\\+\\\[Int1 and_eq:and:\\]|c_Int1__and_eq_and" } } */
/* { dg-final { scan-assembler  "\\-\\\[Int1 or_eq:or:\\]|i_Int1__or_eq_or" } } */
/* { dg-final { scan-assembler  "\\-\\\[Int1 not:xor:\\]|i_Int1__not_xor" } } */
/* { dg-final { scan-assembler  "\\-\\\[Int1 bitand:bitor:\\]|i_Int1__bitand_bitor" } } */
/* { dg-final { scan-assembler  "\\-\\\[Int1 compl:xor_eq:\\]|i_Int1__compl_xor_eq" } } */
/* { dg-final { scan-assembler  "\\-\\\[Int1 not_eq\\]|i_Int1__not_eq" } } */
