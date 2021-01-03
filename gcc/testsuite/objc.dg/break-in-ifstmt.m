/* { dg-do compile } */
/* { dg-additional-options "-Wno-objc-root-class" } */

@interface foo
- (void) test;
@end

@implementation foo
-(void) test {
  if (1) {
        break;	/* { dg-error "break" } */
        }
}
@end

