/* Bug 14610 */
/* { dg-do run { target ia64-*-* } } */
/* { dg-options "-minline-int-divide-max-throughput" } */

extern void abort(void);
volatile int j = 30;

int main(void)
{
  if (29 % j != 29) abort();
  if (30 % j != 0)  abort();
  return 0;
}
