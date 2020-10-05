/* PR target/91441 */
/* { dg-do compile  } */
/* { dg-require-effective-target no_fsanitize_address }*/
/* { dg-options "--param asan-stack=1 -fsanitize=kernel-address" } */

int *bar(int *);
int *f( int a)
{
  return bar(&a);
}
/* { dg-warning ".'-fsanitize=kernel-address' with stack protection is not supported without '-fasan-shadow-offset=' for this target" "" { target *-*-* } 0 } */
