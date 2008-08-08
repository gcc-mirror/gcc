/* Test ICE on defining function with a name previously declared as a
   nonfunction.  Bug 28299 from Bernhard Fischer <aldot@gcc.gnu.org>.
   Reduced testcase from Volker Reichelt <reichelt@gcc.gnu.org>.  */

/* { dg-do compile } */
/* { dg-options "-Wmissing-prototypes" } */

int foo;
/* { dg-message "note: previous declaration" "previous declaration" { target *-*-* } 8 } */
void foo () {} /* { dg-error "redeclared as different kind of symbol" } */
/* { dg-warning "no previous prototype" "no previous prototype" { target *-*-* } 10 } */
