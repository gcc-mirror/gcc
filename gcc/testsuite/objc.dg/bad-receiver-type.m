/* { dg-do compile } */

@interface A

- (void)test; 

@end

extern int foo();

void baz()
{
    [foo test];	/* { dg-warning "invalid receiver type" } */ 
		/* { dg-error "cannot convert to a pointer type" "" { target *-*-* } 13 } */
}
