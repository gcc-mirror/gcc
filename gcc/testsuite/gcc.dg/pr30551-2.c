/* PR 30551 -Wmain is not enabled by default. */
/* { dg-do compile } */
/* { dg-options "" } */

void main(char a) {} /* { dg-bogus "first argument of .main. should be .int." } */
/* { dg-bogus ".main. takes only zero or two arguments" "" { target *-*-* } 5 } */ 
/* { dg-bogus "return type of .main. is not .int." "" { target *-*-* } 5 } */ 

