// { dg-options "-fnon-call-exceptions" }

struct Ax {
  int n, a[];
};

#if __SIZEOF_INT__ < 4
int i = 12345;
#else
int i = 12345678;
#endif
int main() {
  static Ax s{456, i};
  ((s.a[0]) ? (void)0 : (void)0);
}
