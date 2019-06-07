/* PR sanitizer/88619 */
/* { dg-do compile { target fstack_protector } } */
/* { dg-options "-fstack-protector-strong -fsanitize=address" } */

typedef int A __attribute__((aligned (64)));

int
main ()
{
  A b;
  int *p = &b;
  *(p - 1) = 123;
  void *p2 = __builtin_alloca (b);
}
