int foo();

struct wigner_d
  {
  void recurse () {
    int dd;
    for (int j=0; j<=1; ++j) {
#pragma omp parallel
      dd=5;
      }
    }
  };

template<typename T> void rotate_alm(T arg)
  {
  wigner_d rec;
  rec.recurse();
#pragma omp parallel
    foo();
  }

template void rotate_alm(float arg);
template void rotate_alm(double arg);
