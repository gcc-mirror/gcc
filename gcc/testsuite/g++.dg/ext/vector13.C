// PR c++/35096
// { dg-do compile }

typedef const int X __attribute((vector_size(8)));
extern const int x[] __attribute((vector_size(8)));
X x[] = { 5 };
