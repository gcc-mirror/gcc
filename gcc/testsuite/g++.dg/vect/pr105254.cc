/* { dg-do compile } */
/* { dg-additional-options "-fvect-cost-model=dynamic -mcpu=zeus" { target aarch64*-*-* } } */

template <typename>
struct complex;

template <>
struct complex<double> {
  void operator+= (complex r) { v_ += r.v_; }
  _Complex v_;
};

template <typename T>
void
bar (T);

void
foo (complex<double> *x)
{
  complex<double> s = {0.0};

  for (int i = 0; i < 16; ++i)
    s += x[i];

  bar<complex<double> > (s);
}
