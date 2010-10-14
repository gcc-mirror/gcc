/* This program checks for proper declaration of property. */
/* { dg-do compile } */

@interface Bar
@end

@implementation Bar
@property int foo; /* { dg-error "no declaration of property 'foo' found in the interface" } */
@end
