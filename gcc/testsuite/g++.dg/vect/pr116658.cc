// { dg-do compile }
// { dg-require-effective-target c++11 }
// { dg-additional-options "-O3" }
// { dg-additional-options "-mavx512f" { target avx512f } }

struct bb {
  bb operator+=(bb bc) {
      bd[0] += bc.bd[0];
    return *this;
  }
  bb operator-=(bb bc) {
      bd[0] -= bc.bd[0];
    return *this;
  }
  bb operator*=(double be) {
      bd[0] *= be;
    return *this;
  }
  double bd[1];
};

bb operator+(bb n, bb v) {
  bb bf = n;
  return bf += v;
}

bb operator-(bb n, bb v) {
  bb bf = n;
  return bf -= v;
}
bb operator*(double n, bb v) {
  bb bf = v;
  return bf *= n;
}

using az = bb;
struct cc {
  void apply(bb *ci) {
  bb xm[1];
  for (int cm = 0; cm < 2; ++cm) {
    az cn, co = cv[cm] * xm[0];
    ci[cm] = cn + co;
    ci[-1] = cn - co;
  }
  }
  double *cu;
  double *cv;
};
void dc(unsigned de, int di, az *dk, az *dl, cc dh) {
  for (int c; c < 1024; ++c) {
    if (de & 1)
      dh.apply(dk);
    if (de & 2)
      dh.apply(dl);
    dk += di;
    dl += di;
  }
}
