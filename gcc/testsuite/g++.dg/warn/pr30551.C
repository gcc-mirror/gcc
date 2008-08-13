// PR 30551 -Wmain is enabled by default.
// { dg-do compile }
// { dg-options "" }

int main(char a) {} /* { dg-warning "warning: first argument of .*main.* should be .int." } */
/* { dg-warning "warning: .*main.* takes only zero or two arguments" "" { target *-*-* } 5 } */ 
