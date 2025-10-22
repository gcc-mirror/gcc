/* { dg-do compile } */
/* { dg-options "-march=rv64g -mabi=lp64d -fdump-rtl-expand" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

struct Smeu_1f {
  union {
  } e1;
  struct {
    float f;
    union {
    } e;
  } fe;
  union {
  } e2;
};
struct Smeu_1f echo_Smeu_1f(int i, float f, struct Smeu_1f s) /* { dg-warning "ABI for flattened empty union and zero length array changed in GCC 16" } */ {
  return s;
}

/* { dg-final { scan-rtl-dump {\(set \(reg.*:SF \d+ .*\)[[:space:]]+\(reg.*:SF \d+ fa1 \[ s\+4 \]\)\)} "expand" } } */
/* { dg-final { scan-rtl-dump {\(set \(reg.*:SF \d+ fa0\)[[:space:]]+\(reg.*:SF \d+ \[ <retval>\+4 \]\)\)} "expand" } } */
