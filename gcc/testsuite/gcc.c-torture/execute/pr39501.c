/* { dg-options "-ffast-math" } */
#define min1(a,b) ((a) < (b) ? (a) : (b))
#define max1(a,b) ((a) > (b) ? (a) : (b))

#define min2(a,b) ((a) <= (b) ? (a) : (b))
#define max2(a,b) ((a) >= (b) ? (a) : (b))

#define F(type,n)						\
  type __attribute__((noinline)) type##_##n(type a, type b)	\
  {								\
    return n(a, b);						\
  }

F(float,min1)
F(float,min2)
F(float,max1)
F(float,max2)

F(double,min1)
F(double,min2)
F(double,max1)
F(double,max2)

int main()
{
  if (float_min1(0.f, -1.f) != -1.f) abort();
  if (float_min1(-1.f, 0.f) != -1.f) abort();
  if (float_min1(0.f, 1.f)  != 0.f)  abort();
  if (float_min1(1.f, 0.f)  != 0.f)  abort();
  if (float_min1(-1.f, 1.f) != -1.f) abort();
  if (float_min1(1.f, -1.f) != -1.f) abort();
  
  if (float_max1(0.f, -1.f) != 0.f)  abort();
  if (float_max1(-1.f, 0.f) != 0.f)  abort();
  if (float_max1(0.f, 1.f)  != 1.f)  abort();
  if (float_max1(1.f, 0.f)  != 1.f)  abort();
  if (float_max1(-1.f, 1.f) != 1.f)  abort();
  if (float_max1(1.f, -1.f) != 1.f)  abort();
  
  if (float_min2(0.f, -1.f) != -1.f) abort();
  if (float_min2(-1.f, 0.f) != -1.f) abort();
  if (float_min2(0.f, 1.f)  != 0.f)  abort();
  if (float_min2(1.f, 0.f)  != 0.f)  abort();
  if (float_min2(-1.f, 1.f) != -1.f) abort();
  if (float_min2(1.f, -1.f) != -1.f) abort();
  
  if (float_max2(0.f, -1.f) != 0.f)  abort();
  if (float_max2(-1.f, 0.f) != 0.f)  abort();
  if (float_max2(0.f, 1.f)  != 1.f)  abort();
  if (float_max2(1.f, 0.f)  != 1.f)  abort();
  if (float_max2(-1.f, 1.f) != 1.f)  abort();
  if (float_max2(1.f, -1.f) != 1.f)  abort();
  
  if (double_min1(0., -1.) != -1.) abort();
  if (double_min1(-1., 0.) != -1.) abort();
  if (double_min1(0., 1.)  != 0.)  abort();
  if (double_min1(1., 0.)  != 0.)  abort();
  if (double_min1(-1., 1.) != -1.) abort();
  if (double_min1(1., -1.) != -1.) abort();
  
  if (double_max1(0., -1.) != 0.)  abort();
  if (double_max1(-1., 0.) != 0.)  abort();
  if (double_max1(0., 1.)  != 1.)  abort();
  if (double_max1(1., 0.)  != 1.)  abort();
  if (double_max1(-1., 1.) != 1.)  abort();
  if (double_max1(1., -1.) != 1.)  abort();
  
  if (double_min2(0., -1.) != -1.) abort();
  if (double_min2(-1., 0.) != -1.) abort();
  if (double_min2(0., 1.)  != 0.)  abort();
  if (double_min2(1., 0.)  != 0.)  abort();
  if (double_min2(-1., 1.) != -1.) abort();
  if (double_min2(1., -1.) != -1.) abort();
  
  if (double_max2(0., -1.) != 0.)  abort();
  if (double_max2(-1., 0.) != 0.)  abort();
  if (double_max2(0., 1.)  != 1.)  abort();
  if (double_max2(1., 0.)  != 1.)  abort();
  if (double_max2(-1., 1.) != 1.)  abort();
  if (double_max2(1., -1.) != 1.)  abort();
  
  exit(0);
}
