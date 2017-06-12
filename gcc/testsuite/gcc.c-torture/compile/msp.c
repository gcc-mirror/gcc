/* { dg-additional-options "-DSTACK_SIZE=[dg-effective-target-value stack_size]" { target { stack_size } } } */

#ifdef STACK_SIZE
# define A_SIZE (STACK_SIZE/sizeof(int))
#else
# define A_SIZE 16384
#endif
foo ()
{
  int a[A_SIZE];
  bar (a);
}
