// This is a comment with some UTF-8 non-ASCII characters: áéíóú.
// { dg-do compile { target c++11 } }
// { dg-options "-Wno-narrowing -Wconversion" }

const signed char a[] = {
#embed __FILE__
};	// { dg-warning "conversion from 'int' to 'const signed char' changes value from '\[12]\[0-9]\[0-9]' to '-\[0-9]\[0-9]*'" }
