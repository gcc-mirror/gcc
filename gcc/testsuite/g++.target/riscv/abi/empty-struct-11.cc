/* { dg-do compile } */
/* { dg-options "-march=rv64g -mabi=lp64d -fdump-rtl-expand" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

struct Sm1ae_1f {
  struct {
  } e1[1];
  struct {
    float f;
    struct {
    } e[1];
  } fe;
  struct {
  } e2[1];
};
struct Sm1ae_1f echo_Sm1ae_1f(int i, float f, struct Sm1ae_1f s) {
  return s;
}

/* { dg-final { scan-rtl-dump {\(set \(mem.*:DI .*\[.* s\+0 .*\]\)[[:space:]]+\(reg.*:DI \d+ a1\)\)} "expand" } } */
/* { dg-final { scan-rtl-dump {\(set \(mem.*:DI .*\[.* s\+8 .*\]\)[[:space:]]+\(reg.*:DI \d+ a2\)\)} "expand" } } */
/* { dg-final { scan-rtl-dump {\(set \(reg.*:DI \d+ a0 .*\)[[:space:]]+\(subreg:DI \(reg.*:TI \d+ \[ <retval> \]\) 0\)\)} "expand" } } */
/* { dg-final { scan-rtl-dump {\(set \(reg.*:DI \d+ a1 .*\)[[:space:]]+\(subreg:DI \(reg.*:TI \d+ \[ <retval> \]\) 8\)\)} "expand" } } */
