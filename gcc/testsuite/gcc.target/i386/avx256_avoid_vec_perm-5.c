/* { dg-do compile } */
/* { dg-options "-march=sierraforest -Ofast" } */
/* { dg-final { scan-assembler-not {(?n)vpermpd.*%ymm} } } */

typedef struct {
  unsigned short m1, m2, m3, m4;
} the_struct_t;
typedef struct {
  double m1, m2, m3, m4, m5;
} the_struct2_t;

double bar1 (the_struct2_t*);

double foo (double* k, unsigned int n, the_struct_t* the_struct) {
  unsigned int u;
  the_struct2_t result;
  for (u=0; u < n; u++, k--) {
    result.m1 += (*k)*the_struct[u].m1;
    result.m2 += (*k)*the_struct[u].m2;
    result.m3 += (*k)*the_struct[u].m3;
    result.m4 += (*k)*the_struct[u].m4;
  }
  return bar1 (&result);
}
