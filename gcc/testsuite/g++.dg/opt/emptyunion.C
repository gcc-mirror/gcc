// PR optimization/11059
// This testcase ICEd because clear_by_pieces was called with zero length.
// { dg-do compile }
// { dg-options "-O2" }

union uni {};

int main() {
  uni *h;

  h = (uni *)new uni();
}

