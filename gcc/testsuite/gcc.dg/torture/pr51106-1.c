/* PR target/51106 */
/* { dg-do compile } */
/* { dg-skip-if "RTL error" { "*-*-*" } { "-fno-fat-lto-objects" } { "" } } */

int
foo (int x)
{
  asm goto ("" : : "i" (x) : : lab); /* { dg-error "impossible constraint" } */
  /* { dg-warning "probably does not match constraints" "" { target *-*-* } .-1 } */
  return 1;
lab:
  return 0;
}

