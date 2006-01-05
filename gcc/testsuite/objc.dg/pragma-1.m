/* { dg-do compile { target *-*-darwin* } } */
/* ??? Is there a better pragma that is handled for all targets, not
   handled by the preprocessor, that would be better for testing here?  */

@interface a {}
#pragma mark --- Output ---
@end
