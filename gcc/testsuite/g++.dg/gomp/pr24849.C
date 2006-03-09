/* { dg-do compile } */

template<typename T> struct Healpix_Map {
  T *map;
  int npix_;

  void Import_nograde (const Healpix_Map<T> &orig) {
#pragma omp parallel
{
    int m;
#pragma omp for schedule (dynamic)
    for (m=0; m<npix_; ++m) map[m] = orig.map[m];
}
    }
  };

void foo(Healpix_Map<int> &a, Healpix_Map<int> &b) {
  a.Import_nograde(b);
  }
