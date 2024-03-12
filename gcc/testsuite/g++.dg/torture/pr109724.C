// { dg-do compile }

double &SkTMax(double &a, double &b) { return a ?: b; }
struct SkDPoint {
  double fX;
};
struct SkDCubic {
  SkDPoint &operator[](int n) { return fPts[n]; }
  SkDPoint fPts[4];
};
struct SkDCurve {
  SkDCubic fCubic;
  SkDPoint &operator[](int n) { return fCubic[n]; }
};
struct SkDCurveSweep {
  SkDCurve fCurve;
};
int endsIntersect_rPts;
double endsIntersect_maxX;
struct SkOpAngle {
  void endsIntersect();
  SkDCurveSweep fPart;
} endsIntersect_rh;
void SkOpAngle::endsIntersect() {
  for (int index = 0;; ++index) {
    SkDCurve &curve = index ? endsIntersect_rh.fPart.fCurve : fPart.fCurve;
    for (int idx2 = 0; endsIntersect_rPts; ++idx2) {
      SkDPoint &__trans_tmp_1 = curve[idx2];
      endsIntersect_maxX = SkTMax(endsIntersect_maxX, __trans_tmp_1.fX);
    }
  }
}
