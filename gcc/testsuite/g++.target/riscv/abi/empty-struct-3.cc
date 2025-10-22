/* { dg-do compile } */
/* { dg-options "-march=rv64g -mabi=lp64d -fdump-rtl-expand" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

struct Sme_1f {
  struct {
  } e1;
  struct {
    float f;
    struct {
    } e;
  } fe;
  struct {
  } e2;
};
struct Sme_1f echo_Sme_1f(int i, float f, struct Sme_1f s) {
  return s;
}

/* { dg-final { scan-rtl-dump {\(set \(reg.*:SF \d+ .*\)[[:space:]]+\(reg.*:SF \d+ fa1 \[ s\+4 \]\)\)} "expand" } } */
/* { dg-final { scan-rtl-dump {\(set \(reg.*:SF \d+ fa0\)[[:space:]]+\(reg.*:SF \d+ \[ <retval>\+4 \]\)\)} "expand" } } */
