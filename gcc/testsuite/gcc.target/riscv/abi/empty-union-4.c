/* { dg-do compile } */
/* { dg-options "-march=rv64g -mabi=lp64d -fdump-rtl-expand" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

struct Smeu_2f {
  union {
  } e1;
  struct {
    float f;
    float g;
    union {
    } e;
  } fe;
  union {
  } e2;
};
struct Smeu_2f echo_Smeu_2f(int i, float f, struct Smeu_2f s) /* { dg-warning "ABI for flattened empty union and zero length array changed in GCC 16" } */ {
  return s;
}

/* { dg-final { scan-rtl-dump {\(set \(reg.*:SF \d+ .*\)[[:space:]]+\(reg.*:SF \d+ fa1 \[ s \]\)\)} "expand" } } */
/* { dg-final { scan-rtl-dump {\(set \(reg.*:SF \d+ .*\)[[:space:]]+\(reg.*:SF \d+ fa2 \[ s\+4 \]\)\)} "expand" } } */
/* { dg-final { scan-rtl-dump {\(set \(reg.*:SF \d+ fa0\)[[:space:]]+\(reg.*:SF \d+ \[ <retval> \]\)\)} "expand" } } */
/* { dg-final { scan-rtl-dump {\(set \(reg.*:SF \d+ fa1\)[[:space:]]+\(reg.*:SF \d+ \[ <retval>\+4 \]\)\)} "expand" } } */
