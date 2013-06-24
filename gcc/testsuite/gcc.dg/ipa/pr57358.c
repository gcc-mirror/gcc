/* { dg-do compile } */
/* { dg-options "-O2" } */

struct t { void (*func)(void*); };
void test_func(struct t* a) __attribute__((optimize("O0")));
void test_func(struct t* a)
{
  a->func(0);
}
