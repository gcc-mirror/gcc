/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

__attribute__((always_inline))
static inline void f(int n, int (*a())[n])
{
  /* Unused 'a'.  */
}

void g(void)
{
  int (*a())[1];
  f(1, a);
}
