/* PR target/96260 */
/* { dg-do compile } */
/* { dg-options "--param asan-stack=1 -fsanitize=kernel-address -fasan-shadow-offset=0x100000" } */
/* { dg-skip-if "no address sanitizer" { no_fsanitize_address } } */

int *bar(int *);
int *f( int a)
{
  return bar(&a);
}
