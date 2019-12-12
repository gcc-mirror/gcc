// { dg-options "-fnon-call-exceptions" }

struct Ax {
  int n, a[];
};

int i = 12345678;
int main() {
  static Ax s{456, i};
  ((s.a[0]) ? (void)0 : (void)0);
}
