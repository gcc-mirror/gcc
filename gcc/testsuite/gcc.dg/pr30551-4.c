/* PR 30551 -Wmain is enabled by -pedantic-errors and can be disabled. */
/* { dg-do compile } */
/* { dg-options "-pedantic-errors -Wno-main" } */

void main(char a) {} /* { dg-bogus "first argument of .main. should be .int." } */
/* { dg-bogus ".main. takes only zero or two arguments" "" { target *-*-* } 5 } */ 
/* { dg-bogus "return type of .main. is not .int." "" { target *-*-* } 5 } */ 

