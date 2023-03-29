/* PR middle-end/103859 */
/* { dg-do compile } */
/* { dg-options "" } */

typedef struct dcmplx dcmplx;

struct dcmplx {
  double re;
  double im;
};

dcmplx horner(int n, dcmplx p[n], dcmplx x);

int main(void)
{
  int i, n;
  dcmplx x[n + 1], f[n + 1];

  horner(n + 1, f, x[i]);

  return 0;
}

