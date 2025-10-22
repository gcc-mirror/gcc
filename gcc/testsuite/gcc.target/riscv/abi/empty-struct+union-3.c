/* { dg-do compile } */
/* { dg-options "-march=rv64g -mabi=lp64d -fdump-rtl-expand" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

struct Smu2e_1f {
  union {
    struct {
    } e1, e2;
  } u1;
  struct {
    float f;
    union {
      struct {
      } e1, e2;
    } u;
  } ue;
  union {
    struct {
    } e1, e2;
  } u2;
};
struct Smu2e_1f echo_Smu2e_1f(int i, float f, struct Smu2e_1f s) {
  return s;
}

/* { dg-final { scan-rtl-dump {\(set \(reg.*:SF \d+ .*\)[[:space:]]+\(reg.*:SF \d+ fa1 \[ s \]\)\)} "expand" } } */
/* { dg-final { scan-rtl-dump {\(set \(reg.*:SF \d+ fa0\)[[:space:]]+\(reg.*:SF \d+ \[ <retval> \]\)\)} "expand" } } */
