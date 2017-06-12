/* PR 30551 -Wmain is enabled by -Wall.  */
/* { dg-do compile } */
/* { dg-options "-Wall" } */
/* { dg-skip-if "-Wmain not enabled with -Wall on SPU" { spu-*-* } } */
void main(char a) {} /* { dg-warning "first argument of .main. should be .int." "int" } */
/* { dg-warning ".main. takes only zero or two arguments" "zero or two" { target *-*-* } .-1 } */ 
/* { dg-warning "return type of .main. is not .int." "return type" { target *-*-* } .-2 } */ 
