/* { dg-options "(-mmicromips)" } */

int MICROMIPS
foo (int a)
{
  return a;
}

int NOMICROMIPS
foo2 (int a)
{
  return a;
}
