/* { dg-do compile { target c++17 } } */

const static char c0 = u8'';		// { dg-error "empty character" }
const static char c1 = u8'ab';  	// { dg-warning "multi-character character constant" }
const static char c2 = u8'\u0124';	// { dg-warning "multi-character character constant" }
const static char c3 = u8'\U00064321';  // { dg-warning "multi-character character constant" }
