/* { dg-do compile } */
/* { dg-options "-march=rv64g -mabi=lp64d -fdump-rtl-expand" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

struct Sm0ae_1f {
  struct {
  } e1[0];
  struct {
    float f;
    struct {
    } e[0];
  } fe;
  struct {
  } e2[0];
};
struct Sm0ae_1f echo_Sm0ae_1f(int i, float f, struct Sm0ae_1f s) {
  return s;
}

/* { dg-final { scan-rtl-dump {\(set \(reg.*:SF \d+ .*\)[[:space:]]+\(reg.*:SF \d+ fa1 \[ s \]\)\)} "expand" } } */
/* { dg-final { scan-rtl-dump {\(set \(reg.*:SF \d+ fa0\)[[:space:]]+\(reg.*:SF \d+ \[ <retval> \]\)\)} "expand" } } */
