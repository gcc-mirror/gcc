/* { dg-do compile } */

@interface Foo
@property ( readonly, getter = HELLO, setter = THERE : ) int value; /* { dg-error ".readonly. attribute conflicts with .setter. attribute" } */
@end
