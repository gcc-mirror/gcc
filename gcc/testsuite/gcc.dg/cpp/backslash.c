/* Test backslash newline with and without trailing spaces.  */

#define alpha(a, b, c) \
	a, \
	b, \
	c

/* Note the trailing whitespace on the next three lines.  */
#define beta(a, b, c) \ 
	a, \	
	b, \ 	
	c

/* { dg-warning "separated by space" "space" { target *-*-* } 9 } */
/* { dg-warning "separated by space" "tab" { target *-*-* } 10 } */
/* { dg-warning "separated by space" "space and tab" { target *-*-* } 11 } */

int x[] = {
   alpha(1, 2, 3),
   beta(4, 5, 6)
};
