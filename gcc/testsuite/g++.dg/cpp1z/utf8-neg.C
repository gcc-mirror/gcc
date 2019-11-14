/* { dg-do compile { target c++17 } } */

const static char c0 = u8'';		// { dg-error "empty character" }
const static char c1 = u8'ab';  	// { dg-error "character constant too long for its type" }
const static char c2 = u8'\u0124';	// { dg-error "character constant too long for its type" }
const static char c3 = u8'\U00064321';  // { dg-error "character constant too long for its type" }
