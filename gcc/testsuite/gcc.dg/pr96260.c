/* PR target/96260 */
/* { dg-do compile } */
/* { dg-require-effective-target no_fsanitize_address }*/
/* { dg-options "--param asan-stack=1 -fsanitize=kernel-address -fasan-shadow-offset=0x100000" } */

int *bar(int *);
int *f( int a)
{
  return bar(&a);
}
