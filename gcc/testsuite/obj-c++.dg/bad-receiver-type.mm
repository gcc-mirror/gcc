// { dg-do compile } 
// { dg-options "" }
// { dg-additional-options "-Wno-objc-root-class" }

@interface A

- (void)test; 

@end

extern int foo();

void baz()
{
    [foo test];	/* { dg-warning "invalid receiver type" } */
}
