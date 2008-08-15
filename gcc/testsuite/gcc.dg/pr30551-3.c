/* PR 30551 -Wmain is enabled by -pedantic-errors. */
/* { dg-do compile } */
/* { dg-options "-pedantic-errors" } */
/* { dg-skip-if "-Wmain not enabled with -pedantic on SPU" { spu-*-* } } */
void main(char a) {} /* { dg-error "first argument of .main. should be .int." } */
/* { dg-error ".main. takes only zero or two arguments" "" { target *-*-* } 5 } */ 
/* { dg-error "return type of .main. is not .int." "" { target *-*-* } 5 } */ 
