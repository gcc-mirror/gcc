/* getter/setter cannot be specified in an interface. */
/* { dg-do compile } */

@interface Foo
@property ( readonly, getter = HELLO, setter = THERE : ) int value;
@end	/* { dg-warning "getter = \\'HELLO\\' may not be specified in an interface" } */ 
	/* { dg-warning "setter = \\'THERE\\:\\' may not be specified in an interface" "" { target *-*-* } 6 } */
