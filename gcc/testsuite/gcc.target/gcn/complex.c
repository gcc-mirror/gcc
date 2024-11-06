// { dg-do run }
// { dg-options "-std=c99 -O3" }

#include <stdlib.h>
#include <stdbool.h>

#define COUNT 1000
#define MAX 1000
#define ALIGNMENT (2*1024*1024) // 2MB

_Complex double conj(_Complex double);
_Complex float conjf(_Complex float);

unsigned int device = 0;

// cmul

void cmulF(float *td, float *te, float *tf, float *tg, int tas)
{
  typedef _Complex float complexT;
  int array_size = tas/2;
  complexT *d = (complexT*)(td);
  complexT *e = (complexT*)(te);
  complexT *f = (complexT*)(tf);
  for (int i = 0; i < array_size; i++)
    {
      d[i] = e[i] * f[i];
    }
}

__attribute__((optimize("no-tree-vectorize")))
bool cmulFcheck(float *td, float *te, float *tf, float *tg, int tas)
{
  for (int i = 0; i < tas/2; ++i)
    {
      float a = te[i*2];
      float b = te[i*2+1];
      float c = tf[i*2];
      float d = tf[i*2+1];
      if (td[i*2] != a*c-b*d || td[i*2+1] != a*d+b*c)
        return false;
    }
  return true;
}

void cmulD(double *td, double *te, double *tf, double *tg, int tas)
{
  typedef _Complex double complexT;
  int array_size = tas/2;
  complexT *d = (complexT*)(td);
  complexT *e = (complexT*)(te);
  complexT *f = (complexT*)(tf);
  for (int i = 0; i < array_size; i++)
    {
      d[i] = e[i] * f[i];
    }
}

__attribute__((optimize("no-tree-vectorize")))
bool cmulDcheck(double *td, double *te, double *tf, double *tg, int tas)
{
  for (int i = 0; i < tas/2; ++i)
    {
      double a = te[i*2];
      double b = te[i*2+1];
      double c = tf[i*2];
      double d = tf[i*2+1];
      if (td[i*2] != a*c-b*d || td[i*2+1] != a*d+b*c)
        return false;
    }
  return true;
}


// cmul_conj

void cmul_conjF(float *td, float *te, float *tf, float *tg, int tas)
{
  typedef _Complex float complexT;
  int array_size = tas/2;
  complexT *d = (complexT*)(td);
  complexT *e = (complexT*)(te);
  complexT *f = (complexT*)(tf);
  for (int i = 0; i < array_size; i++)
    {
      d[i] = e[i] * conj(f[i]);
    }
}

__attribute__((optimize("no-tree-vectorize")))
bool cmul_conjFcheck(float *td, float *te, float *tf, float *tg, int tas)
{
  for (int i = 0; i < tas/2; ++i)
    {
      float a = te[i*2];
      float b = te[i*2+1];
      float c = tf[i*2];
      float d = tf[i*2+1];
      if (td[i*2] != a*c+b*d || td[i*2+1] != b*c-a*d)
        return false;
    }
  return true;
}

void cmul_conjD(double *td, double *te, double *tf, double *tg, int tas)
{
  typedef _Complex double complexT;
  int array_size = tas/2;
  complexT *d = (complexT*)(td);
  complexT *e = (complexT*)(te);
  complexT *f = (complexT*)(tf);
  for (int i = 0; i < array_size; i++)
    {
      d[i] = e[i] * conj(f[i]);
    }
}

__attribute__((optimize("no-tree-vectorize")))
bool cmul_conjDcheck(double *td, double *te, double *tf, double *tg, int tas)
{
  for (int i = 0; i < tas/2; ++i)
    {
      double a = te[i*2];
      double b = te[i*2+1];
      double c = tf[i*2];
      double d = tf[i*2+1];
      if (td[i*2] != a*c+b*d || td[i*2+1] != b*c-a*d)
        return false;
    }
  return true;
}


// addsub

void addsubF(float *td, float *te, float *tf, float *tg, int tas)
{
  typedef _Complex float complexT;
  int array_size = tas/2;
  complexT *d = (complexT*)(td);
  complexT *e = (complexT*)(te);
  complexT *f = (complexT*)(tf);
  for (int i = 0; i < array_size; i++)
    {
      d[i] = e[i] - conjf(f[i]);
    }
}

__attribute__((optimize("no-tree-vectorize")))
bool addsubFcheck(float *td, float *te, float *tf, float *tg, int tas)
{
  for (int i = 0; i < tas/2; ++i)
    {
      float a = te[i*2];
      float b = te[i*2+1];
      float c = tf[i*2];
      float d = tf[i*2+1];
      if (td[i*2] != a-c || td[i*2+1] != b+d)
        return false;
    }
  return true;
}

void addsubD(double *td, double *te, double *tf, double *tg, int tas)
{
  typedef _Complex double complexT;
  int array_size = tas/2;
  complexT *d = (complexT*)(td);
  complexT *e = (complexT*)(te);
  complexT *f = (complexT*)(tf);
  for (int i = 0; i < array_size; i++)
    {
      d[i] = e[i] - conj(f[i]);
    }
}

__attribute__((optimize("no-tree-vectorize")))
bool addsubDcheck(double *td, double *te, double *tf, double *tg, int tas)
{
  for (int i = 0; i < tas/2; ++i)
    {
      double a = te[i*2];
      double b = te[i*2+1];
      double c = tf[i*2];
      double d = tf[i*2+1];
      if (td[i*2] != a-c || td[i*2+1] != b+d)
        return false;
    }
  return true;
}


// fmaddsub

void fmaddsubF(float *td, float *te, float *tf, float *tg, int tas)
{
  int array_size = tas/2;
  for (int i = 0; i < array_size; i++)
    {
      td[i*2] = te[i*2]*tf[i*2]-tg[i*2];
      td[i*2+1] = te[i*2+1]*tf[i*2+1]+tg[i*2+1];
    }
}

__attribute__((optimize("no-tree-vectorize")))
bool fmaddsubFcheck(float *td, float *te, float *tf, float *tg, int tas)
{
  for (int i = 0; i < tas/2; ++i)
    {
      float a = te[i*2];
      float b = te[i*2+1];
      float c = tf[i*2];
      float d = tf[i*2+1];
      float e = tg[i*2];
      float f = tg[i*2+1];
      if (td[i*2] != a*c-e || td[i*2+1] != b*d+f)
        return false;
    }
  return true;
}

void fmaddsubD(double *td, double *te, double *tf, double *tg, int tas)
{
  int array_size = tas/2;
  for (int i = 0; i < array_size; i++)
    {
      td[i*2] = te[i*2]*tf[i*2]-tg[i*2];
      td[i*2+1] = te[i*2+1]*tf[i*2+1]+tg[i*2+1];
    }
}

__attribute__((optimize("no-tree-vectorize")))
bool fmaddsubDcheck(double *td, double *te, double *tf, double *tg, int tas)
{
  for (int i = 0; i < tas/2; ++i)
    {
      double a = te[i*2];
      double b = te[i*2+1];
      double c = tf[i*2];
      double d = tf[i*2+1];
      double e = tg[i*2];
      double f = tg[i*2+1];
      if (td[i*2] != a*c-e || td[i*2+1] != b*d+f)
        return false;
    }
  return true;
}


// fmsubadd

void fmsubaddF(float *td, float *te, float *tf, float *tg, int tas)
{
  int array_size = tas/2;
  for (int i = 0; i < array_size; i++)
    {
      td[i*2] = te[i*2]*tf[i*2]+tg[i*2];
      td[i*2+1] = te[i*2+1]*tf[i*2+1]-tg[i*2+1];
    }
}

__attribute__((optimize("no-tree-vectorize")))
bool fmsubaddFcheck(float *td, float *te, float *tf, float *tg, int tas)
{
  for (int i = 0; i < tas/2; ++i)
    {
      float a = te[i*2];
      float b = te[i*2+1];
      float c = tf[i*2];
      float d = tf[i*2+1];
      float e = tg[i*2];
      float f = tg[i*2+1];
      if (td[i*2] != a*c+e || td[i*2+1] != b*d-f)
        return false;
    }
  return true;
}

void fmsubaddD(double *td, double *te, double *tf, double *tg, int tas)
{
  int array_size = tas/2;
  for (int i = 0; i < array_size; i++)
    {
      td[i*2] = te[i*2]*tf[i*2]+tg[i*2];
      td[i*2+1] = te[i*2+1]*tf[i*2+1]-tg[i*2+1];
    }
}

__attribute__((optimize("no-tree-vectorize")))
bool fmsubaddDcheck(double *td, double *te, double *tf, double *tg, int tas)
{
  for (int i = 0; i < tas/2; ++i)
    {
      double a = te[i*2];
      double b = te[i*2+1];
      double c = tf[i*2];
      double d = tf[i*2+1];
      double e = tg[i*2];
      double f = tg[i*2+1];
      if (td[i*2] != a*c+e || td[i*2+1] != b*d-f)
        return false;
    }
  return true;
}


// cadd90

void cadd90F(float *td, float *te, float *tf, float *tg, int tas)
{
  int array_size = tas/2;
  for (int i = 0; i < array_size; i++)
    {
      td[i*2] = te[i*2] - tf[i*2+1];
      td[i*2+1] = te[i*2+1] + tf[i*2];
    }
}

__attribute__((optimize("no-tree-vectorize")))
bool cadd90Fcheck(float *td, float *te, float *tf, float *tg, int tas)
{
  for (int i = 0; i < tas/2; ++i)
    {
      float a = te[i*2];
      float b = te[i*2+1];
      float c = tf[i*2];
      float d = tf[i*2+1];
      if (td[i*2] != a-d || td[i*2+1] != b+c)
        return false;
    }
  return true;
}

void cadd90D(double *td, double *te, double *tf, double *tg, int tas)
{
  int array_size = tas/2;
  for (int i = 0; i < array_size; i++)
    {
      td[i*2] = te[i*2] - tf[i*2+1];
      td[i*2+1] = te[i*2+1] + tf[i*2];
    }
}

__attribute__((optimize("no-tree-vectorize")))
bool cadd90Dcheck(double *td, double *te, double *tf, double *tg, int tas)
{
  for (int i = 0; i < tas/2; ++i)
    {
      double a = te[i*2];
      double b = te[i*2+1];
      double c = tf[i*2];
      double d = tf[i*2+1];
      if (td[i*2] != a-d || td[i*2+1] != b+c)
        return false;
    }
  return true;
}

// cadd270

void cadd270F(float *td, float *te, float *tf, float *tg, int tas)
{
  int array_size = tas/2;
  for (int i = 0; i < array_size; i++)
    {
      td[i*2] = te[i*2] + tf[i*2+1];
      td[i*2+1] = te[i*2+1] - tf[i*2];
    }
}

__attribute__((optimize("no-tree-vectorize")))
bool cadd270Fcheck(float *td, float *te, float *tf, float *tg, int tas)
{
  for (int i = 0; i < tas/2; ++i)
    {
      float a = te[i*2];
      float b = te[i*2+1];
      float c = tf[i*2];
      float d = tf[i*2+1];
      if (td[i*2] != a+d || td[i*2+1] != b-c)
        return false;
    }
  return true;
}

void cadd270D(double *td, double *te, double *tf, double *tg, int tas)
{
  int array_size = tas/2;
  for (int i = 0; i < array_size; i++)
    {
      td[i*2] = te[i*2] + tf[i*2+1];
      td[i*2+1] = te[i*2+1] - tf[i*2];
    }
}

__attribute__((optimize("no-tree-vectorize")))
bool cadd270Dcheck(double *td, double *te, double *tf, double *tg, int tas)
{
  for (int i = 0; i < tas/2; ++i)
    {
      double a = te[i*2];
      double b = te[i*2+1];
      double c = tf[i*2];
      double d = tf[i*2+1];
      if (td[i*2] != a+d || td[i*2+1] != b-c)
        return false;
    }
  return true;
}


// cmla

void cmlaF(float *td, float *te, float *tf, float *tg, int tas)
{
  typedef _Complex float complexT;
  int array_size = tas/2;
  complexT *d = (complexT*)(td);
  complexT *e = (complexT*)(te);
  complexT *f = (complexT*)(tf);
  complexT *g = (complexT*)(tg);
  for (int i = 0; i < array_size; i++)
    {
      d[i] = e[i] * f[i] + g[i];
    }
}

__attribute__((optimize("no-tree-vectorize")))
bool cmlaFcheck(float *td, float *te, float *tf, float *tg, int tas)
{
  for (int i = 0; i < tas/2; ++i)
    {
      float a = te[i*2];
      float b = te[i*2+1];
      float c = tf[i*2];
      float d = tf[i*2+1];
      float e = tg[i*2];
      float f = tg[i*2+1];
      if (td[i*2] != a*c-b*d+e || td[i*2+1] != a*d+b*c+f)
        return false;
    }
  return true;
}

void cmlaD(double *td, double *te, double *tf, double *tg, int tas)
{
  typedef _Complex double complexT;
  int array_size = tas/2;
  complexT *d = (complexT*)(td);
  complexT *e = (complexT*)(te);
  complexT *f = (complexT*)(tf);
  complexT *g = (complexT*)(tg);
  for (int i = 0; i < array_size; i++)
    {
      d[i] = e[i] * f[i] + g[i];
    }
}

__attribute__((optimize("no-tree-vectorize")))
bool cmlaDcheck(double *td, double *te, double *tf, double *tg, int tas)
{
  for (int i = 0; i < tas/2; ++i)
    {
      double a = te[i*2];
      double b = te[i*2+1];
      double c = tf[i*2];
      double d = tf[i*2+1];
      double e = tg[i*2];
      double f = tg[i*2+1];
      if (td[i*2] != a*c-b*d+e || td[i*2+1] != a*d+b*c+f)
        return false;
    }
  return true;
}


// cmls

void cmlsF(float *td, float *te, float *tf, float *tg, int tas)
{
  typedef _Complex float complexT;
  int array_size = tas/2;
  complexT *d = (complexT*)(td);
  complexT *e = (complexT*)(te);
  complexT *f = (complexT*)(tf);
  complexT *g = (complexT*)(tg);
  for (int i = 0; i < array_size; i++)
    {
      d[i] = e[i] * f[i] - g[i];
    }
}

__attribute__((optimize("no-tree-vectorize")))
bool cmlsFcheck(float *td, float *te, float *tf, float *tg, int tas)
{
  for (int i = 0; i < tas/2; ++i)
    {
      float a = te[i*2];
      float b = te[i*2+1];
      float c = tf[i*2];
      float d = tf[i*2+1];
      float e = tg[i*2];
      float f = tg[i*2+1];
      if (td[i*2] != a*c-b*d-e || td[i*2+1] != a*d+b*c-f)
        return false;
    }
  return true;
}

void cmlsD(double *td, double *te, double *tf, double *tg, int tas)
{
  typedef _Complex double complexT;
  int array_size = tas/2;
  complexT *d = (complexT*)(td);
  complexT *e = (complexT*)(te);
  complexT *f = (complexT*)(tf);
  complexT *g = (complexT*)(tg);
  for (int i = 0; i < array_size; i++)
    {
      d[i] = e[i] * f[i] - g[i];
    }
}

__attribute__((optimize("no-tree-vectorize")))
bool cmlsDcheck(double *td, double *te, double *tf, double *tg, int tas)
{
  for (int i = 0; i < tas/2; ++i)
    {
      double a = te[i*2];
      double b = te[i*2+1];
      double c = tf[i*2];
      double d = tf[i*2+1];
      double e = tg[i*2];
      double f = tg[i*2+1];
      if (td[i*2] != a*c-b*d-e || td[i*2+1] != a*d+b*c-f)
        return false;
    }
  return true;
}


typedef void(*runF)(float *td, float *te, float *tf, float *tg, int tas);
typedef void(*runD)(double *td, double *te, double *tf, double *tg, int tas);
typedef bool(*checkF)(float *td, float *te, float *tf, float *tg, int tas);
typedef bool(*checkD)(double *td, double *te, double *tf, double *tg, int tas);

typedef struct
{
  runF rF;
  runD rD;
  checkF cF;
  checkD cD;
} operation;

operation ops[] = {
  {cmulF, cmulD, cmulFcheck, cmulDcheck},
  {cmul_conjF, cmul_conjD, cmul_conjFcheck, cmul_conjDcheck},
  {addsubF, addsubD, addsubFcheck, addsubDcheck},
  {fmaddsubF, fmaddsubD, fmaddsubFcheck, fmaddsubDcheck},
  {fmsubaddF, fmsubaddD, fmsubaddFcheck, fmsubaddDcheck},
  {cadd90F, cadd90D, cadd90Fcheck, cadd90Dcheck},
  {cadd270F, cadd270D, cadd270Fcheck, cadd270Dcheck},
  {cmlaF, cmlaD, cmlaFcheck, cmlaDcheck},
  {cmlsF, cmlsD, cmlsFcheck, cmlsDcheck}
};

void testF(operation* op)
{
  float* td;
  float* te;
  float* tf;
  float* tg;
  int array_size = COUNT;
  td = (float*)malloc(sizeof(float)*array_size);
  te = (float*)malloc(sizeof(float)*array_size);
  tf = (float*)malloc(sizeof(float)*array_size);
  tg = (float*)malloc(sizeof(float)*array_size);
  float* dd = td;
  float* ee = te;
  float* ff = tf;
  float* gg = tg;
  for (int i = 0; i < COUNT; ++i)
    {
      te[i] = (float)(rand() % MAX);
      tf[i] = (float)(rand() % MAX);
      tg[i] = (float)(rand() % MAX);
    }
  op->rF(td, te, tf, tg, COUNT);
  if (!op->cF(td, te, tf, tg, COUNT))
    abort();
}

void testD(operation* op)
{
  double* td;
  double* te;
  double* tf;
  double* tg;
  int array_size = COUNT;
  td = (double*)malloc(sizeof(double)*array_size);
  te = (double*)malloc(sizeof(double)*array_size);
  tf = (double*)malloc(sizeof(double)*array_size);
  tg = (double*)malloc(sizeof(double)*array_size);
  double* dd = td;
  double* ee = te;
  double* ff = tf;
  double* gg = tg;
  for (int i = 0; i < COUNT; ++i)
    {
      te[i] = (double)(rand() % MAX);
      tf[i] = (double)(rand() % MAX);
      tg[i] = (double)(rand() % MAX);
    }
  op->rD(td, te, tf, tg, COUNT);
  if (!op->cD(td, te, tf, tg, COUNT))
    abort();
}

int main()
{
   for (int i = 0; i < 9; ++i)
    {
      testF(&ops[i]);
      testD(&ops[i]);
    }
}

