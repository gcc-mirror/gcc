extern double fabs (double);
extern double sqrt (double);

typedef struct complexm {
  double re,im;
} complex;

static complex
setCom (double r, double i)
{
  complex ct;
  ct.re=fabs(r)<1E-300?0.0:r;
  ct.im=fabs(i)<1E-300?0.0:i;
  return ct;
}

static complex
csqrt_crash (double x)
{
  return (x>=0) ? setCom(sqrt(x),0) : setCom(0,sqrt(-x));
}
