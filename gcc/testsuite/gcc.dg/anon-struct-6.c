/* Test diagnostics for structure member with no type specifier or
   declarator.  Test with no special options.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "" } */

struct s {
  int a;
  const;
};
/* { dg-warning "warning: useless type qualifier in empty declaration" "empty" { target *-*-* } 9 } */
/* { dg-warning "warning: empty declaration" "empty 2" { target *-*-* } 9 } */
