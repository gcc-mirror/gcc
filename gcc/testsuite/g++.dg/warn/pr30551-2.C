// PR 30551 -Wmain is enabled by -pedantic/-pedantic-errors.
// { dg-do compile }
// { dg-options "-pedantic-errors" }

int main(char a) {} /* { dg-error "error: first argument of .*main.* should be .int." } */
/* { dg-error "error: .*main.* takes only zero or two arguments" "" { target *-*-* } 5 } */ 
