/* { dg-do compile } */
@interface Fraction
-(void) setNumerator: (int) :(int) ; /* { dg-error "expected identifier" } */
@end
