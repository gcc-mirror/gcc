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

float __attribute__ ((noinline)) 
cosatanf_(float x)
{
    float atg = atanf(x);
    return cosf(atg) + atg;
}

double __attribute__ ((noinline)) 
cosatan_(double x)
{
    double atg = atan(x);
    return cos(atg) + atg;
}

long double __attribute__ ((noinline)) 
cosatanl_(long double x)
{
    long double atg = atanl(x);
    return cosl(atg) + atg;
}

float __attribute__ ((noinline)) 
sinatanf_(float x)
{
    float atg = atanf(x);
    return sinf(atg) + atg;
}

double __attribute__ ((noinline)) 
sinatan_(double x)
{
    double atg = atan(x);
    return sin(atg) + atg;
}

long double __attribute__ ((noinline)) 
sinatanl_(long double x)
{
    long double atg = atanl(x);
    return sinl(atg) + atg;
}

/* There should be calls to both sin and atan */
/* { dg-final { scan-tree-dump "cos " "optimized" } } */
/* { dg-final { scan-tree-dump "sin " "optimized" } } */
/* { dg-final { scan-tree-dump "atan " "optimized" } } */
/* { dg-final { scan-tree-dump "cosf " "optimized" } } */
/* { dg-final { scan-tree-dump "sinf " "optimized" } } */
/* { dg-final { scan-tree-dump "atanf " "optimized" } } */
/* { dg-final { scan-tree-dump "cosl " "optimized" } } */
/* { dg-final { scan-tree-dump "sinl " "optimized" } } */
/* { dg-final { scan-tree-dump "atanl " "optimized" } } */
