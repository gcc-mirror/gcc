/* { dg-do compile { target c++17 } } */

const static char c0 = u8'';		// { dg-error "empty character" }
const static char c1 = u8'ab';  	// { dg-error "multi-character literal cannot have an encoding prefix" }
const static char c2 = u8'\u0124';	// { dg-error "character not encodable in a single code unit" }
const static char c3 = u8'\U00064321';  // { dg-error "character not encodable in a single code unit" }
