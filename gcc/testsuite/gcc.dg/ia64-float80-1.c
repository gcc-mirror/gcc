/* Bug 14610 */
/* { dg-do run { target ia64-*-* } } */

extern void abort(void);
volatile __float80 x = 30.0;

int main(void)
{
  double d = x;
  if (d != 30.0) abort ();
  return 0;
}
