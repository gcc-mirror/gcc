/* PR rtl-optimization/102842 */
/* { dg-do compile } */
/* { dg-options "-fPIC  -O2 -fno-omit-frame-pointer -mthumb -march=armv7-a+fp" } */

struct Plane {
  using T = float;
  T *Row();
};
using ImageF = Plane;
long long Mirror_x;
struct EnsurePaddingInPlaceRowByRow {
  void Process() {
    switch (strategy_) {
    case kSlow:
      float *row = img_.Row();
      long long xsize = x1_;
      while (Mirror_x >= xsize)
        if (Mirror_x)
          Mirror_x = 2 * xsize - 1;
      *row = Mirror_x;
    }
  }
  ImageF img_;
  unsigned x1_;
  enum { kSlow } strategy_;
};
void FinalizeImageRect() {
  EnsurePaddingInPlaceRowByRow ensure_padding;
  ensure_padding.Process();
}
