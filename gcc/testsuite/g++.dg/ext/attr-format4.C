// PR c++/116175
// { dg-do compile { target c++11 } }
// { dg-options "-Wformat" }

template <typename ...T>
int foo (T ...args, const char *fmt, ...)
[[gnu::format (printf, 1 + sizeof... (T), 2 + sizeof... (T))]];

int a = foo <> ("%d", 1);
int b = foo <int, int, int, int, int> (1, 2, 3, 4, 5, "%d", 1);
int c = foo <> ("%f", 1);	// { dg-warning "format '%f' expects argument of type 'double', but argument 2 has type 'int'" }
int d = foo <int, int, int, int, int> (1, 2, 3, 4, 5, "%f", 1);	// { dg-warning "format '%f' expects argument of type 'double', but argument 7 has type 'int'" }
