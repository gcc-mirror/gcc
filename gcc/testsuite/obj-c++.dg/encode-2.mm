/* { dg-do compile } */

template <class T>
struct Vec {
 T x, y;
 int z;
};

typedef struct {
  Vec<double> dvec;
  Vec<float> fvec;
  float fscalar;
  double dscalar;
  Vec<char> chVec;
  int iscalar;
} anonymous;

Vec<double> dd;

const char *enc = @encode(Vec<float>);
const char *enc2 = @encode(Vec<double>);
const char *enc3 = @encode(anonymous);

/* { dg-final { scan-assembler "{Vec<float>=ffi}" } }  */
/* { dg-final { scan-assembler "{Vec<double>=ddi}" } }  */
/* { dg-final { scan-assembler "{?={Vec<double>=ddi}{Vec<float>=ffi}fd{Vec<char>=cci}i}" } }  */
