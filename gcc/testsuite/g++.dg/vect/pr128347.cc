// { dg-do compile }

struct vector {
  ~vector();
};
int expand_dx;
template <int SIZE> struct Tuple {
  template <typename IdxT> int &operator[](IdxT i) { return mm[i]; }
  int mm[SIZE];
};
struct Vec3 : Tuple<3> {
  void x();
  void operator-=(Vec3 v1) {
    mm[0] -= v1[0];
    mm[1] -= v1[1];
  }
};
struct BBox {
  Vec3 extents() {
    Vec3 __trans_tmp_2, result(mMax);
    result -= mMin;
    __trans_tmp_2 = result;
    return __trans_tmp_2;
  }
  void expand() {
    for (int i = 0; i < 3; ++i)
      mMin[i] -= mMax[i] += expand_dx;
  }
  Vec3 mMin, mMax;
};
int size();
void TestBody() {
  vector points;
  BBox region;
  region.expand();
  if (size())
    region.extents().x();
}
