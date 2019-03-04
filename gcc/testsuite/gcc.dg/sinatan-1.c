/* { dg-do run { target c99_runtime } } */
/* { dg-options "-Ofast" } */
/* { dg-add-options ieee } */

extern float sinf (float);
extern float cosf (float);
extern float atanf (float);
extern float sqrtf (float);
extern float nextafterf (float, float);
extern double sin (double);
extern double cos (double);
extern double atan (double);
extern double sqrt (double);
extern double nextafter (double, double);
extern long double sinl (long double);
extern long double cosl (long double);
extern long double atanl (long double);
extern long double sqrtl (long double);
extern long double nextafterl (long double, long double);

extern void abort ();

double __attribute__ ((noinline, optimize("Ofast")))
sinatan (double x)
{
    return sin (atan (x));
}

double __attribute__ ((noinline, optimize("Ofast")))
cosatan (double x)
{
    return cos (atan (x));
}

float __attribute__ ((noinline, optimize("Ofast")))
sinatanf(float x)
{
    return sinf (atanf (x));
}

float __attribute__ ((noinline, optimize("Ofast")))
cosatanf(float x)
{
    return cosf (atanf (x));
}

long double __attribute__ ((noinline, optimize("Ofast")))
sinatanl (long double x)
{
    return sinl (atanl (x));
}

long double __attribute__ ((noinline, optimize("Ofast")))
cosatanl (long double x)
{
    return cosl (atanl (x));
}

int
main()
{
    /* Get first x such that 1 + x*x will overflow */
    float fc = nextafterf (sqrtf (__FLT_MAX__ - 1), __FLT_MAX__);
    double c = nextafter (sqrt (__DBL_MAX__ - 1), __DBL_MAX__);
    long double lc = nextafterl (sqrtl (__LDBL_MAX__ - 1), __LDBL_MAX__);

    /*  Force move from FPU to memory, otherwise comparison may
        fail due to possible more accurate registers (see 387)  */
    volatile float fy;
    volatile double y;
    volatile long double ly;

    fy = sinatanf (fc);
    y = sinatan (c);
    ly = sinatanl (lc);

    if (fy != 1.f || y != 1 || ly != 1.L)
        abort ();

    fy = cosatanf (fc);
    y = cosatan (c);
    ly = cosatanl (lc);

    if (fy != 0.f || y != 0. || ly != 0.L)
        abort ();

    fy = sinatanf (-fc);
    y = sinatan (-c);
    ly = sinatanl (-lc);

    if (fy != -1.f || y != -1. || ly != -1.L)
        abort ();
    
    fy = cosatanf (-fc);
    y = cosatan (-c);
    ly = cosatanl (-lc);

    if (fy != 0.f || y != 0. || ly != 0.L)
        abort ();

    return 0;
}
