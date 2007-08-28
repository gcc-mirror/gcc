/* { dg-do compile } */
typedef float FloatVect __attribute__((__vector_size__(16)));
const FloatVect Foo = { 250000000.0, 0.0, 0.0, 0.0 };
