/* PR 30551 -Wmain is enabled by -pedantic. */
/* { dg-do compile } */
/* { dg-options "-pedantic" } */
/* { dg-skip-if "-Wmain not enabled with -pedantic on SPU" { spu-*-* } } */
void main(char a) {} /* { dg-warning "first argument of .main. should be .int." } */
/* { dg-warning ".main. takes only zero or two arguments" "" { target *-*-* } 5 } */ 
/* { dg-warning "return type of .main. is not .int." "" { target *-*-* } 5 } */ 
