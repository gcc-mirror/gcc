/* { dg-do run } */
/* { dg-options "-O" } */

typedef int vec __attribute__((vector_size(4*sizeof(int))));

__attribute__((noinline,noclone))
void f (vec *x) {
  *x = (*x & 1) ^ 1;
}

int main() {
  vec x = { 1, 2, 3, 4 };
  f(&x);
  if (x[0] != 0 || x[1] != 1)
    __builtin_abort();
  return 0;
}
