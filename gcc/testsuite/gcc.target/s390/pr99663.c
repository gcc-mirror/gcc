/* { dg-do compile } */
/* { dg-options "-mtune=z15 -march=z13 -mzarch -O2 -fno-stack-protector -fPIC" } */

typedef struct {
  int exponent;
  unsigned short lsu[];
} decNumber;
decNumber decDivideOp_lhs;
short decDivideOp_varbuff;
void decDivideOp(decNumber *rhs) {
  short *msu1;
  int exponent;
  unsigned short *source;
  for (; source >= decDivideOp_lhs.lsu; source--, msu1--)
    *msu1 = *source;
  for (;;)
    if (exponent)
      if (decDivideOp_varbuff)
        exponent = rhs->exponent;
}
