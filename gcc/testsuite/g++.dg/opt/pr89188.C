// PR target/89188
// { dg-do compile { target c++11 } }
// { dg-options "-Og -flive-range-shrinkage -fnon-call-exceptions" }

struct Ax {
  int n, a[];
};

int i = 12345678;
int main() {
  static Ax s{456, i};
  ((s.a[0]) ? (void)0 : (void)0);
}
