/* { dg-do compile } */
/* { dg-options "-Ofast -fdump-tree-optimized" } */

extern float sinf (float);
extern float cosf (float);
extern float atanf (float);
extern double sin (double);
extern double cos (double);
extern double atan (double);
extern long double sinl (long double);
extern long double cosl (long double);
extern long double atanl (long double);

double __attribute__ ((noinline))
sinatan_ (double x)
{
    return sin (atan (x));
}

double __attribute__ ((noinline))
cosatan_ (double x)
{
    return cos (atan (x));
}

float __attribute__ ((noinline))
sinatanf_(float x)
{
    return sinf (atanf (x));
}

float __attribute__ ((noinline))
cosatanf_(float x)
{
    return cosf (atanf (x));
}

long double __attribute__ ((noinline))
sinatanl_ (long double x)
{
    return sinl (atanl (x));
}

long double __attribute__ ((noinline))
cosatanl_ (long double x)
{
    return cosl (atanl (x));
}

/* There must be no calls to sin, cos, or atan */
/* {dg-final { scan-tree-dump-not "sin " "optimized" } } */
/* {dg-final { scan-tree-dump-not "cos " "optimized" } } */
/* {dg-final { scan-tree-dump-not "atan " "optimized" }} */
/* {dg-final { scan-tree-dump-not "sinf " "optimized" } } */
/* {dg-final { scan-tree-dump-not "cosf " "optimized" } } */
/* {dg-final { scan-tree-dump-not "atanf " "optimized" }} */
/* {dg-final { scan-tree-dump-not "sinl " "optimized" } } */
/* {dg-final { scan-tree-dump-not "cosl " "optimized" } } */
/* {dg-final { scan-tree-dump-not "atanl " "optimized" }} */
