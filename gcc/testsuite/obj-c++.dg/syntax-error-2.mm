/* Recover gracefully from a syntax error.  */
// { dg-additional-options "-Wno-objc-root-class" }

@implementation Whatever  /* { dg-warning "cannot find interface declaration for .Whatever." } */

- (void) function
{
	if( 1 )
	{
	else   /* { dg-error "expected .\}. before .else." } */
	{
	}
}

- (void) another {}

@end
