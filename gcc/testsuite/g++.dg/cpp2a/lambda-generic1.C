// P0428R2
// { dg-do compile { target c++14 } }

int i = [](int i, auto a) { return i; }(3, 4);
int j = []<class T>(T t, int i) { return i; }(3, 4);		  // { dg-error "lambda templates are only available with" "" { target c++17_down } }
int k[2] = { 5, 6 };
int l = []<typename T>(T *p) { return *p; }(k);			  // { dg-error "lambda templates are only available with" "" { target c++17_down } }
int m = []<typename T, int N>(T (&a)[N]) { return a[N - 1]; }(k); // { dg-error "lambda templates are only available with" "" { target c++17_down } }
int n = []<typename T>(T a, auto b) { return a + b; }(7, 8);	  // { dg-error "lambda templates are only available with" "" { target c++17_down } }
