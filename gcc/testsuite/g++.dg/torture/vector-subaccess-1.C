/* PR c++/89224 */
/* { dg-additional-options "-Wno-psabi" } */

/* The access of `vector[i]` has the same qualifiers as the original
   vector which was missing. */

typedef __attribute__((vector_size(16))) unsigned char  Int8x8_t;

template <class T>
void g(T &x) {
    __builtin_abort();
}
template <class T>
void g(const T &x) {
  __builtin_exit(0);
}
void f(const Int8x8_t x) {
  g(x[0]);
}
int main(void)
{
    Int8x8_t x ={};
    f(x);
}
