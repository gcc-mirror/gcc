// { dg-do compile } 
// { dg-options "" }

@interface A

- (void)test; 

@end

extern int foo();

void baz()
{
    [foo test];	/* { dg-warning "invalid receiver type" } */
}
