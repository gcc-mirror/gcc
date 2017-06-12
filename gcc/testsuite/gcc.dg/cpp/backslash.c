/* Test backslash newline with and without trailing spaces.  */

#define alpha(a, b, c) \
	a, \
	b, \
	c

/* Note the trailing whitespace on the first three lines of beta def.  */
/* { dg-warning "separated by space" "space" { target *-*-* } .+3 } */
/* { dg-warning "separated by space" "tab" { target *-*-* } .+3 } */
/* { dg-warning "separated by space" "space and tab" { target *-*-* } .+3 } */
#define beta(a, b, c) \ 
	a, \	
	b, \ 	
	c

int x[] = {
   alpha(1, 2, 3),
   beta(4, 5, 6)
};
