/* { dg-do compile } */

@interface Bar
@end

@implementation Bar
@property int FooBar; /* { dg-error {property declaration not in '@interface', '@protocol' or 'category' context} } */
@end
