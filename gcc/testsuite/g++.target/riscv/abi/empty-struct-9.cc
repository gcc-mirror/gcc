/* { dg-do compile } */
/* { dg-options "-march=rv64g -mabi=lp64d -fdump-rtl-expand" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

struct S1ae_1f {
  struct {
  } e1[1];
  float f;
};
struct S1ae_1f echo_S1ae_1f(int i, float f, struct S1ae_1f s) {
  return s;
}

/* { dg-final { scan-rtl-dump {\(set \(mem.*:DI .*\[.* s\+0 .*\]\)[[:space:]]+\(reg.*:DI \d+ a1\)\)} "expand" } } */
/* { dg-final { scan-rtl-dump {\(set \(reg.*:DI \d+ a0\)[[:space:]]+\(reg.*:DI \d+ \[ <retval> \]\)\)} "expand" } } */
