/* { dg-do compile } */
/* { dg-options "-march=rv64g -mabi=lp64d -fdump-rtl-expand" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

struct S2eu_2f {
  union {
  } e1;
  float f;
  float g;
};
struct S2eu_2f echo_S2eu_2f(int i, float f, struct S2eu_2f s) /* { dg-warning "ABI for flattened empty union and zero length array changed in GCC 16" } */ {
  return s;
}

/* { dg-final { scan-rtl-dump {\(set \(reg.*:SF \d+ .*\)[[:space:]]+\(reg.*:SF \d+ fa1 \[ s\+4 \]\)\)} "expand" } } */
/* { dg-final { scan-rtl-dump {\(set \(reg.*:SF \d+ .*\)[[:space:]]+\(reg.*:SF \d+ fa2 \[ s\+8 \]\)\)} "expand" } } */
/* { dg-final { scan-rtl-dump {\(set \(reg.*:SF \d+ fa0\)[[:space:]]+\(reg.*:SF \d+ \[ <retval>\+4 \]\)\)} "expand" } } */
/* { dg-final { scan-rtl-dump {\(set \(reg.*:SF \d+ fa1\)[[:space:]]+\(reg.*:SF \d+ \[ <retval>\+8 \]\)\)} "expand" } } */
