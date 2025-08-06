// C++26 P2843R3 - Preprocessing is never undefined
// Test that form-feed followed by non-whitespace
// in line comments are accepted.
// { dg-do compile }
// { dg-options "-pedantic-errors -Wall -W" }

// 
int a;
//   	
int b;
//  comment
int c;
