/* { dg-do run } */
/* { dg-options "-O2" } */

extern double fabs(double);
extern float fabsf(float);
extern void abort(void);


double test1(double x)
{
  return fabs(-x);
}

float test1f(float x)
{
  return fabsf(-x);
}

double test2(double x)
{
  return fabs(fabs(x));
}

float test2f(float x)
{
  return fabsf(fabsf(x));
}

double test3(double x, double y)
{
  return fabs(x*-y);
}

float test3f(float x, float y)
{
  return fabsf(x*-y);
}

double test4(double x, double y)
{
  return fabs(x/-y);
}

float test4f(float x, float y)
{
  return fabsf(x/-y);
}

int main()
{
  if (test1(1.0) != 1.0)
    abort();
  if (test1(2.0) != 2.0)
    abort();
  if (test1(0.0) != 0.0)
    abort();
  if (test1(-1.0) != 1.0)
    abort();
  if (test1(-2.0) != 2.0)
    abort();

  if (test1f(1.0f) != 1.0f)
    abort();
  if (test1f(2.0f) != 2.0f)
    abort();
  if (test1f(0.0f) != 0.0f)
    abort();
  if (test1f(-1.0f) != 1.0f)
    abort();
  if (test1f(-2.0f) != 2.0f)
    abort();

  if (test2(1.0) != 1.0)
    abort();
  if (test2(2.0) != 2.0)
    abort();
  if (test2(0.0) != 0.0)
    abort();
  if (test2(-1.0) != 1.0)
    abort();
  if (test2(-2.0) != 2.0)
    abort();

  if (test2f(1.0f) != 1.0f)
    abort();
  if (test2f(2.0f) != 2.0f)
    abort();
  if (test2f(0.0f) != 0.0f)
    abort();
  if (test2f(-1.0f) != 1.0f)
    abort();
  if (test2f(-2.0f) != 2.0f)
    abort();

  if (test3(1.0,1.0) != 1.0)
    abort();
  if (test3(1.0,-1.0) != 1.0)
    abort();
  if (test3(1.0,2.0) != 2.0)
    abort();
  if (test3(1.0,-2.0) != 2.0)
    abort();
  if (test3(2.0,1.0) != 2.0)
    abort();
  if (test3(2.0,-1.0) != 2.0)
    abort();
  if (test3(2.0,2.0) != 4.0)
    abort();
  if (test3(2.0,-2.0) != 4.0)
    abort();
  if (test3(-2.0,1.0) != 2.0)
    abort();
  if (test3(-2.0,-1.0) != 2.0)
    abort();
  if (test3(-2.0,2.0) != 4.0)
    abort();
  if (test3(-2.0,-2.0) != 4.0)
    abort();

  if (test3f(1.0f,1.0f) != 1.0f)
    abort();
  if (test3f(1.0f,-1.0f) != 1.0f)
    abort();
  if (test3f(1.0f,2.0f) != 2.0f)
    abort();
  if (test3f(1.0f,-2.0f) != 2.0f)
    abort();
  if (test3f(2.0f,1.0f) != 2.0f)
    abort();
  if (test3f(2.0f,-1.0f) != 2.0f)
    abort();
  if (test3f(2.0f,2.0f) != 4.0f)
    abort();
  if (test3f(2.0f,-2.0f) != 4.0f)
    abort();
  if (test3f(-2.0f,1.0f) != 2.0f)
    abort();
  if (test3f(-2.0f,-1.0f) != 2.0f)
    abort();
  if (test3f(-2.0f,2.0f) != 4.0f)
    abort();
  if (test3f(-2.0f,-2.0f) != 4.0f)
    abort();

  if (test4(1.0,1.0) != 1.0)
    abort();
  if (test4(1.0,-1.0) != 1.0)
    abort();
  if (test4(-1.0,1.0) != 1.0)
    abort();
  if (test4(-1.0,-1.0) != 1.0)
    abort();
  if (test4(6.0,3.0) != 2.0)
    abort();
  if (test4(6.0,-3.0) != 2.0)
    abort();
  if (test4(-6.0,3.0) != 2.0)
    abort();
  if (test4(-6.0,-3.0) != 2.0)
    abort();

  if (test4f(1.0f,1.0f) != 1.0f)
    abort();
  if (test4f(1.0f,-1.0f) != 1.0f)
    abort();
  if (test4f(-1.0f,1.0f) != 1.0f)
    abort();
  if (test4f(-1.0f,-1.0f) != 1.0f)
    abort();
  if (test4f(6.0f,3.0f) != 2.0f)
    abort();
  if (test4f(6.0f,-3.0f) != 2.0f)
    abort();
  if (test4f(-6.0f,3.0f) != 2.0f)
    abort();
  if (test4f(-6.0f,-3.0f) != 2.0f)
    abort();

  return 0;
}

