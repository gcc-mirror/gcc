/* Testcase for PR c/1501. */
double __complex__
f (void)
{
  return ~(1.0 + 2.0i);
}
