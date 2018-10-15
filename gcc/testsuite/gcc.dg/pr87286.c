/* { dg-options "-Wno-psabi" } */
enum foo { F };
typedef enum foo vec_foo __attribute__((vector_size (16)));
vec_foo add (vec_foo x, vec_foo y) { return x + y; }
