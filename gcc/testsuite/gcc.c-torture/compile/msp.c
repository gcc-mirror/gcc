/* { dg-add-options stack_size } */

void bar (int *);

#ifdef STACK_SIZE
# define A_SIZE (STACK_SIZE/sizeof(int))
#else
# define A_SIZE 16384
#endif
void
foo (void)
{
  int a[A_SIZE];
  bar (a);
}
