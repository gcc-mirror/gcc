// Test that duplicate function parameters are found in declarations.

extern void g0 (int a, int b);
extern void g1 (int a, float b);

extern void f0 (int a,
                int a);  // { dg-error "multiple parameters named 'a'" }
extern void f1 (int a,
                float a);  // { dg-error "multiple parameters named 'a'" }
extern void f3 (int a, int b, int c,
                int a);  // { dg-error "multiple parameters named 'a'" }
extern void f4 (int a, int b, int c,
                int a,
                int a);  // { dg-error "multiple parameters named 'a'" }
extern void f5 (int a, int b, int c, int d, int e, int f, int g, int h,
                int a,
                int i, int j, int k, int l, int m, int n, int o, int p,
                int q, int r, int s, int t, int u, int v, int w, int x, int y,
                int z);  // { dg-error "multiple parameters named 'a'" }

extern void f6 (int a, int, int, int, int, int, int, int, int, int, int,
                int a,
                int, int, int, int, int, int, int, int, int, int, int,
                float, float, float, float, float, float, float, float,
                int);  // { dg-error "multiple parameters named 'a'" }

extern void f7 (void (*a)(int),
                void (*a)(int));  // { dg-error "multiple parameters named 'a'" }
extern void f8 (float (*a)(int),
                int (*a)(float));  // { dg-error "multiple parameters named 'a'" }

extern void f9 (int a,
                int a,
                int a);
// { dg-error "multiple parameters named 'a'" "" { target *-*-* } 34 }

extern void f10 (int a,
                 int b,
                 int c,
                 int c,
                 int b,
                 int a);
// { dg-error "multiple parameters named 'a'" "" { target *-*-* } 42 }
// { dg-error "multiple parameters named 'b'" "" { target *-*-* } 42 }
// { dg-error "multiple parameters named 'c'" "" { target *-*-* } 42 }

class C1 {
 public:
  void C1_g0 (int a, int b);
  void C1_f0 (int a,
              int a);  // { dg-error "multiple parameters named 'a'" }
};

template <class T>
class C2 {
 public:
  void C2_g0 (T a, T b);
  void C2_f0 (T a,
              T a);  // { dg-error "multiple parameters named 'a'" }
};
