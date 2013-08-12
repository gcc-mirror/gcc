// Test that duplicate function parameters are found in declarations.

extern void g0 (int a, int b);
extern void g1 (int a, float b);

extern void f0 (int a,		// { dg-message "previous" }
                int a);		// { dg-error "redefinition" }
extern void f1 (int a,		// { dg-message "previous" }
                float a);	// { dg-error "conflicting" }
extern void f3 (int a, int b, int c, // { dg-message "previous" }
                int a);		// { dg-error "redefinition" }
extern void f4 (int a, int b, int c, // { dg-message "previous" }
                int a,		     // { dg-error "redefinition" }
                int a);		     // { dg-error "redefinition" }
extern void f5 (int a, int b, int c, int d, int e, int f, int g, int h, // { dg-message "previous" }
                int a,							// { dg-error "redefinition" }
                int i, int j, int k, int l, int m, int n, int o, int p,
                int q, int r, int s, int t, int u, int v, int w, int x, int y,
                int z);

extern void f6 (int a, int, int, int, int, int, int, int, int, int, int, // { dg-message "previous" }
                int a,		// { dg-error "redefinition" }
                int, int, int, int, int, int, int, int, int, int, int,
                float, float, float, float, float, float, float, float,
                int);

extern void f7 (void (*a)(int),	  // { dg-message "previous" }
                void (*a)(int));  // { dg-error "redefinition" }
extern void f8 (float (*a)(int),  // { dg-message "previous" }
                int (*a)(float));  // { dg-error "conflicting" }

extern void f9 (int a,		// { dg-message "previous" }
                int a,		// { dg-error "redefinition" }
                int a);		// { dg-error "redefinition" }

extern void f10 (int a,		// { dg-message "previous" }
                 int b,		// { dg-message "previous" }
                 int c,		// { dg-message "previous" }
                 int c,		// { dg-error "redefinition" }
                 int b,		// { dg-error "redefinition" }
                 int a);	// { dg-error "redefinition" }

class C1 {
 public:
  void C1_g0 (int a, int b);
  void C1_f0 (int a,		// { dg-message "previous" }
              int a);		// { dg-error "redefinition" }
};

template <class T>
class C2 {
 public:
  void C2_g0 (T a, T b);
  void C2_f0 (T a,		// { dg-message "previous" }
              T a);		// { dg-error "redefinition" }
};
