// PR 30551 -Wmain is enabled by -pedantic/-pedantic-errors.
// { dg-do compile }
// { dg-options "-pedantic-errors" }
// { dg-skip-if "-Wmain not enabled with -pedantic on SPU" { spu-*-* } } 
int main(char a) {} /* { dg-error "first argument of .*main.* should be .int." "int" } */
/* { dg-error "main.* takes only zero or two arguments" "zero or two" { target *-*-* } .-1 } */ 
