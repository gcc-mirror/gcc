/* { dg-do compile } */

typedef struct Vec {
 double x, y;
 int z;
} xyz_t ;

typedef struct {
  float fscalar;
  double dscalar;
  xyz_t dv;
  int iscalar;
} anonymous;

const char *enc = @encode(xyz_t);
const char *enc2 = @encode(anonymous);

/* { dg-final { scan-assembler "{Vec=ddi}" } }  */
/* { dg-final { scan-assembler "{?=fd{Vec=ddi}i}" } }  */
