/* { dg-do run } */
/* { dg-options "-O2" } */

extern double copysign(double,double);
extern float copysignf(float,float);
extern double fabs(double);
extern float fabsf(float);
extern void abort(void);


double test1(double x, double y)
{
  return copysign(-x,y);
}

float test1f(float x, float y)
{
  return copysignf(-x,y);
}

double test2(double x, double y)
{
  return copysign(fabs(x),y);
}

float test2f(float x, float y)
{
  return copysignf(fabsf(x),y);
}

double test3(double x, double y, double z)
{
  return copysign(x*-y,z);
}

float test3f(float x, float y, float z)
{
  return copysignf(x*-y,z);
}

double test4(double x, double y, double z)
{
  return copysign(x/-y,z);
}

float test4f(float x, float y, float z)
{
  return copysignf(x/-y,z);
}

int main()
{
  if (test1(3.0,2.0) != 3.0)
    abort();
  if (test1(3.0,-2.0) != -3.0)
    abort();
  if (test1(-3.0,2.0) != 3.0)
    abort();
  if (test1(-3.0,-2.0) != -3.0)
    abort();

  if (test1f(3.0f,2.0f) != 3.0f)
    abort();
  if (test1f(3.0f,-2.0f) != -3.0f)
    abort();
  if (test1f(-3.0f,2.0f) != 3.0f)
    abort();
  if (test1f(-3.0f,-2.0f) != -3.0f)
    abort();

  if (test2(3.0,2.0) != 3.0)
    abort();
  if (test2(3.0,-2.0) != -3.0)
    abort();
  if (test2(-3.0,2.0) != 3.0)
    abort();
  if (test2(-3.0,-2.0) != -3.0)
    abort();

  if (test2f(3.0f,2.0f) != 3.0f)
    abort();
  if (test2f(3.0f,-2.0f) != -3.0f)
    abort();
  if (test2f(-3.0f,2.0f) != 3.0f)
    abort();
  if (test2f(-3.0f,-2.0f) != -3.0f)
    abort();

  if (test3(2.0,3.0,4.0) != 6.0)
    abort();
  if (test3(2.0,3.0,-4.0) != -6.0)
    abort();
  if (test3(2.0,-3.0,4.0) != 6.0)
    abort();
  if (test3(2.0,-3.0,-4.0) != -6.0)
    abort();
  if (test3(-2.0,3.0,4.0) != 6.0)
    abort();
  if (test3(-2.0,3.0,-4.0) != -6.0)
    abort();
  if (test3(-2.0,-3.0,4.0) != 6.0)
    abort();
  if (test3(-2.0,-3.0,-4.0) != -6.0)
    abort();

  if (test3f(2.0f,3.0f,4.0f) != 6.0f)
    abort();
  if (test3f(2.0f,3.0f,-4.0f) != -6.0f)
    abort();
  if (test3f(2.0f,-3.0f,4.0f) != 6.0f)
    abort();
  if (test3f(2.0f,-3.0f,-4.0f) != -6.0f)
    abort();
  if (test3f(-2.0f,3.0f,4.0f) != 6.0f)
    abort();
  if (test3f(-2.0f,3.0f,-4.0f) != -6.0f)
    abort();
  if (test3f(-2.0f,-3.0f,4.0f) != 6.0f)
    abort();
  if (test3f(-2.0f,-3.0f,-4.0f) != -6.0f)
    abort();

  if (test4(8.0,2.0,3.0) != 4.0)
    abort();
  if (test4(8.0,2.0,-3.0) != -4.0)
    abort();
  if (test4(8.0,-2.0,3.0) != 4.0)
    abort();
  if (test4(8.0,-2.0,-3.0) != -4.0)
    abort();
  if (test4(-8.0,2.0,3.0) != 4.0)
    abort();
  if (test4(-8.0,2.0,-3.0) != -4.0)
    abort();
  if (test4(-8.0,-2.0,3.0) != 4.0)
    abort();
  if (test4(-8.0,-2.0,-3.0) != -4.0)
    abort();

  if (test4f(8.0f,2.0f,3.0f) != 4.0f)
    abort();
  if (test4f(8.0f,2.0f,-3.0f) != -4.0f)
    abort();
  if (test4f(8.0f,-2.0f,3.0f) != 4.0f)
    abort();
  if (test4f(8.0f,-2.0f,-3.0f) != -4.0f)
    abort();
  if (test4f(-8.0f,2.0f,3.0f) != 4.0f)
    abort();
  if (test4f(-8.0f,2.0f,-3.0f) != -4.0f)
    abort();
  if (test4f(-8.0f,-2.0f,3.0f) != 4.0f)
    abort();
  if (test4f(-8.0f,-2.0f,-3.0f) != -4.0f)
    abort();

  return 0;
}

