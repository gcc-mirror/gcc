/* { dg-options "-w" } */
/* { dg-do compile } */
@implementation A
+B  
+C {} /* { dg-error "expected '\{' before '\\\+' token" } */
@end
