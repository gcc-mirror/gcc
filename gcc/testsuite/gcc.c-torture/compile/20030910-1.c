/* The gimplifier was getting confused when taking the real or
   imaginary component of a complex rvalue.  */

void test()
{
  __complex double dc;
  double d;

  d = __real (dc * dc);
}

