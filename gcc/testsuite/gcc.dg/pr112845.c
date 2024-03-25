/* PR target/112845 */
/* { dg-do compile { target cet } } */
/* { dg-options "-Os -fcf-protection" } */

unsigned long long
foo (void)
{
  return 0xfa1e0ff3ULL << 3;
}
