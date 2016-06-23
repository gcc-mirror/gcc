/* { dg-do compile } */
/* { dg-additional-options "-Wno-psabi" { target { { i?86-*-* x86_64-*-* } && ilp32 } } } */

/* Testcase by Martin Michlmayr <tbm@cyrius.com> */

class Matrix
{
  public:
    float data[4][4] __attribute__ ((__aligned__(16)));
    Matrix operator* (const Matrix matrix) const;
    void makeRotationAboutVector (void);
};
void Matrix::makeRotationAboutVector (void)
{
   Matrix irx;
   *this = irx * (*this);
}
Matrix Matrix::operator* (const Matrix matrix) const
{
  Matrix ret;
  for (int i = 0; i < 4; i++)
    for (int j = 0; j < 4; j++)
      ret.data[j][i] = matrix.data[j][2] + matrix.data[j][3];
  return ret;
}

