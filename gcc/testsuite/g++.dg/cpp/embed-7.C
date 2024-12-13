// This is a comment with some UTF-8 non-ASCII characters: áéíóú.
// { dg-do compile { target c++11 } }
// { dg-options "" } */

const signed char a[] = {
#embed __FILE__
};	// { dg-error "narrowing conversion of '\[12]\[0-9]\[0-9]' from 'int' to 'const signed char'" }
