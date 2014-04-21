// { dg-do compile { target c++11 } }

typedef float X __attribute__ ((vector_size (4 * sizeof (float))));

X x;
X x2{x};
