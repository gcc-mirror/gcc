// { dg-do compile }
// { dg-require-effective-target c++11 }
// { dg-additional-options "-march=znver2" { target x86_64-*-* i?86-*-* } }

typedef double T;
T c, s;
T a[16];
struct Matrix4 {
  Matrix4(){}
  Matrix4(T e, T f, T i, T j) {
    r[1] = r[4] = e;
    r[5] = f;
    r[8] = i;
    r[9] = j;
  }
  Matrix4 operator*(Matrix4 a) {
    return Matrix4(
       r[0] * a.r[4] + r[4] + r[15] + r[6],
       r[1] * a.r[4] + 1 + 2 + 3,  r[0] * r[8] + 1 + 2 + 3,
       r[1] * r[8] + r[1] + r[14] + r[2] * r[3]);
  }
  T r[16] = {};
};
Matrix4 t1, t2;
Matrix4 tt;
Matrix4 getRotAltAzToEquatorial()
{
  t2.r[4] =  0;
  t1.r[1] =  -s;
  t1.r[8] = 0;
  return t1 * t2;
}
