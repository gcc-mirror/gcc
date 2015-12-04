/* Functional tests for the "target" attribute and pragma.  */

/* { dg-do compile } */
/* { dg-require-effective-target target_attribute } */
/* { dg-options "-msoft-float" } */

#pragma GCC target("hard-float")
float p1(float f)
{
  return 2 * f;
}
#pragma GCC reset_options

#pragma GCC target("soft-float")
float p0(float f)
{
  return 2 * f;
}
float p0b(float f)
{
  return 2 * f;
}
#pragma GCC reset_options

__attribute__ ((target("hard-float")))
float a1(float f)
{
  return 2 * f;
}

__attribute__ ((target("hard-float")))
float a1b(float f)
{
  return 2 * f;
}

__attribute__ ((target("hard-float")))
float a1c(float f)
{
  return 2 * f;
}

__attribute__ ((target("hard-float")))
float a1d(float f)
{
  return 2 * f;
}

__attribute__ ((target("soft-float")))
float a0(float f)
{
  return 2 * f;
}

__attribute__ ((target("soft-float")))
float a0b(float f)
{
  return 2 * f;
}

__attribute__ ((target("soft-float")))
float a0c(float f)
{
  return 2 * f;
}

__attribute__ ((target("soft-float")))
float a0d(float f)
{
  return 2 * f;
}

__attribute__ ((target("soft-float")))
float a0e(float f)
{
  return 2 * f;
}

__attribute__ ((target("soft-float")))
float a0f(float f)
{
  return 2 * f;
}

__attribute__ ((target("soft-float")))
float a0g(float f)
{
  return 2 * f;
}

__attribute__ ((target("soft-float")))
float a0h(float f)
{
  return 2 * f;
}

/* { dg-final { scan-assembler-times "\tste\t%f" 5 } } */
