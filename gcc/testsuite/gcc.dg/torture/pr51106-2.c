/* PR target/51106 */
/* { dg-do "compile" } */
/* { dg-skip-if "RTL error" { "*-*-*" } { "-fno-fat-lto-objects" } { "" } } */
/* { dg-skip-if "" { powerpc-ibm-aix* } } */

int
bar (int x)
{
  asm goto ("" : : "i" (x) : : lab); /* { dg-error "impossible constraint" } */
/* { dg-warning "probably doesn.t match constraints" "" { target *-*-* } .-1 } */
  __builtin_unreachable ();
lab:
  return 0;
}

