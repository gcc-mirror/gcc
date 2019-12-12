// PR 30551 -Wmain is enabled by default.
// { dg-do compile }
// { dg-options "" }
int main(char a) {} /* { dg-warning "first argument of .*main.* should be .int." "int" } */
/* { dg-warning "main.* takes only zero or two arguments" "zero or two" { target *-*-* } .-1 } */ 
