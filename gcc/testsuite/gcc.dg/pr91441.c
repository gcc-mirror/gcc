/* PR target/91441 */
/* { dg-do compile  } */
/* { dg-options "--param asan-stack=1 -fsanitize=kernel-address" } */
/* { dg-skip-if "no address sanitizer" { no_fsanitize_address } } */

int *bar(int *);
int *f( int a)
{
  return bar(&a);
}
/* { dg-warning ".'-fsanitize=kernel-address' with stack protection is not supported without '-fasan-shadow-offset=' for this target" "" { target riscv*-*-* } 0 } */
