/* PR 30551 -Wmain is enabled by -pedantic. */
/* { dg-do compile } */
/* { dg-options "-pedantic" } */
void main(char a) {} /* { dg-warning "first argument of .main. should be .int." "int" } */
/* { dg-warning ".main. takes only zero or two arguments" "zero or two" { target *-*-* } .-1 } */ 
/* { dg-warning "return type of .main. is not .int." "return type" { target *-*-* } .-2 } */ 
