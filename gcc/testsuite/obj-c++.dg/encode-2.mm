/* { dg-do compile } */

template <class T>
struct Vec {
 T x, y;
 int z;
};

Vec<double> dd;
const char *enc = @encode(Vec<float>);
const char *enc2 = @encode(Vec<double>);

/* { dg-final { scan-assembler "{Vec<float>=ffi}" { xfail "*-*-*" } } } PR32052 */
/* { dg-final { scan-assembler "{Vec<double>=ddi}" { xfail "*-*-*" } } } PR32052 */
