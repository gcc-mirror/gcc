/* { dg-do compile } */
/* { dg-options "-march=rv64g -mabi=lp64d -fdump-rtl-expand" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

struct Sm1ae_2f {
  struct {
  } e1[1];
  struct {
    float f;
    float g;
    struct {
    } e[1];
  } fe;
  struct {
  } e2[1];
};
struct Sm1ae_2f echo_Sm1ae_2f(int i, float f, struct Sm1ae_2f s) {
  return s;
}

/* { dg-final { scan-rtl-dump {\[.* \.result_ptr\+0 .*\]} "expand" } } */
