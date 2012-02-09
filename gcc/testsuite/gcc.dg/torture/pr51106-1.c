/* PR target/51106 */
/* { dg-do "compile" } */
/* { dg-skip-if "RTL error" { "*-*-*" } { "-fno-fat-lto-objects" } { "" } } */

int
foo (int x)
{
  asm goto ("" : : "i" (x) : : lab); /* { dg-error "impossible constraint" } */
  return 1;
lab:
  return 0;
}

/* { dg-warning "probably doesn.t match constraints" "" { target *-*-* } 8 } */
