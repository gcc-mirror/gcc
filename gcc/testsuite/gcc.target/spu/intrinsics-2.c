/* { dg-do run } */
/* { dg-options "-std=c99" } */
#include <vec_types.h>
extern void abort (void);
extern void exit (int);

typedef union {
  vec_ullong2   vull;
  vec_double2   vd;
  unsigned int  ui[4];
  unsigned long long ull[2];
  double        d[2];
} v128;

static v128 a, b, c, d, a0, b0, a1, b1;
static int samples = 10;
unsigned int seed = 0;

unsigned int rand_local()
{
  seed = seed * 69607 + 54329;
  return (seed);
}

double rand_double(double min, double max)
{
  union {
    unsigned int ui[2];
    double d;
  } x;

  x.ui[0] = (rand_local() & 0x000FFFFF) | 0x3FF00000;
  x.ui[1] = rand_local();
  x.d -= 1.0;
  x.d *= max - min;
  x.d += min;
  return (x.d);
}

vec_double2 rand_vd(double min, double max)
{
  int i;
  static v128 val;

  for (i=0; i<2; i++) val.d[i] = rand_double(min, max);
  return (val.vd);
}

int test_spu_cmpeq()
{
  int i, j;
  unsigned long long exp;

  /* double */
  for (i=0; i<samples; i++) {
    a.vd = rand_vd(-4.0, 4.0);
    b.vd = rand_vd(-4.0, 4.0);
    d.vull = spu_cmpeq(a.vd, b.vd);
    for (j=0; j<2; j++) {
      exp = (a.d[j] == b.d[j]) ? 
	    (((unsigned long long)(0xFFFFFFFF) << 32) 
	     | (unsigned long long)(0xFFFFFFFF)) : 0;
      if (exp != d.ull[j]) abort();
    }
  }

  /* compare zeros  */
    d.vull = spu_cmpeq(a0.vd, b0.vd);
    for (j=0; j<2; j++) {
      exp = (a0.d[j] == b0.d[j]) ? 
	    (((unsigned long long)(0xFFFFFFFF) << 32) 
	     | (unsigned long long)(0xFFFFFFFF)) : 0;
      if (exp != d.ull[j]) abort();
    }

  /* compare NaNs  */
    d.vull = spu_cmpeq(a1.vd, b1.vd);
    for (j=0; j<2; j++) {
      exp = (a1.d[j] == b1.d[j]) ? 
	    (((unsigned long long)(0xFFFFFFFF) << 32) 
	     | (unsigned long long)(0xFFFFFFFF)) : 0;
      if (exp != d.ull[j]) abort();
    }
  return 0;
}

int test_spu_cmpgt()
{   
  int i, j;
  unsigned long long exp;

  /* double */
  for (i=0; i<samples; i++) {
    a.vd = rand_vd(-4.0, 4.0);
    b.vd = rand_vd(-4.0, 4.0);
    d.vull = spu_cmpgt(a.vd, b.vd);
    for (j=0; j<2; j++) {
      exp = (a.d[j] > b.d[j]) ? 
	    (((unsigned long long)(0xFFFFFFFF) << 32) 
	     | (unsigned long long)(0xFFFFFFFF)) : 0;
      if (exp != d.ull[j]) abort();
    } 
  }

  /* compare zeros  */
    d.vull = spu_cmpgt(a0.vd, b0.vd);
    for (j=0; j<2; j++) {
      exp = (a0.d[j] > b0.d[j]) ? 
	    (((unsigned long long)(0xFFFFFFFF) << 32) 
	     | (unsigned long long)(0xFFFFFFFF)) : 0;
      if (exp != d.ull[j]) abort();
    }
  /* compare NaNs  */
    d.vull = spu_cmpgt(a1.vd, b1.vd);
    for (j=0; j<2; j++) {
      exp = (a1.d[j] > b1.d[j]) ? 
	    (((unsigned long long)(0xFFFFFFFF) << 32) 
	     | (unsigned long long)(0xFFFFFFFF)) : 0;
      if (exp != d.ull[j]) abort();
    }
  return 0;
}

int test_spu_cmpabseq()
{   
  int i, j;
  unsigned long long exp;

  /* double */
  for (i=0; i<samples; i++) {
    a.vd = rand_vd(-4.0, 4.0);
    b.vd = rand_vd(-4.0, 4.0);
    d.vull = spu_cmpabseq(a.vd, b.vd);
    for (j=0; j<2; j++) {
      exp = ((a.d[j] == b.d[j]) || (-a.d[j] == b.d[j]) || (a.d[j] == -b.d[j])) ?
	    (((unsigned long long)(0xFFFFFFFF) << 32) 
	     | (unsigned long long)(0xFFFFFFFF)) : 0;
      if (exp != d.ull[j]) abort();
    } 
  }

  /* compare zeros  */
    d.vull = spu_cmpabseq(a0.vd, b0.vd);
    for (j=0; j<2; j++) {
      exp = ((a0.d[j] == b0.d[j]) || (-a0.d[j] == b0.d[j]) || (a0.d[j] == -b0.d[j])) ?
	    (((unsigned long long)(0xFFFFFFFF) << 32) 
	     | (unsigned long long)(0xFFFFFFFF)) : 0;
      if (exp != d.ull[j]) abort();
    }

  /* compare NaNs  */
    d.vull = spu_cmpabseq(a1.vd, b1.vd);
    for (j=0; j<2; j++) {
      exp = ((a1.d[j] == b1.d[j]) || (-a1.d[j] == b1.d[j]) || (a1.d[j] == -b1.d[j])) ?
	    (((unsigned long long)(0xFFFFFFFF) << 32) 
	     | (unsigned long long)(0xFFFFFFFF)) : 0;
      if (exp != d.ull[j]) abort();
    }
  return 0;
}

int test_spu_cmpabsgt()
{
  int i, j;
  unsigned long long exp;
  double abs_a, abs_b;
  
  /* double */
  for (i=0; i<samples; i++) {
    a.vd = rand_vd(-4.0, 4.0);
    b.vd = rand_vd(-4.0, 4.0);
    d.vull = spu_cmpabsgt(a.vd, b.vd);
    for (j=0; j<2; j++) {
      double abs_a = (a.d[j] < 0.0) ? -a.d[j] : a.d[j];
      double abs_b = (b.d[j] < 0.0) ? -b.d[j] : b.d[j];
      exp = (abs_a > abs_b) ? 
	    (((unsigned long long)(0xFFFFFFFF) << 32) 
	     | (unsigned long long)(0xFFFFFFFF)) : 0;
      if (exp != d.ull[j]) abort();
    }
  }

  /* compare zeros  */
    d.vull = spu_cmpabsgt(a0.vd, b0.vd);
    for (j=0; j<2; j++) {
      abs_a = (a0.d[j] < 0.0) ? -a0.d[j] : a0.d[j];
      abs_b = (b0.d[j] < 0.0) ? -b0.d[j] : b0.d[j];
      exp = (abs_a > abs_b) ? 
	    (((unsigned long long)(0xFFFFFFFF) << 32) 
	     | (unsigned long long)(0xFFFFFFFF)) : 0;
      if (exp != d.ull[j]) abort();
    }
  /* compare NaNs  */
    d.vull = spu_cmpabsgt(a1.vd, b1.vd);
    for (j=0; j<2; j++) {
      abs_a = (a1.d[j] < 0.0) ? -a1.d[j] : a1.d[j];
      abs_b = (b1.d[j] < 0.0) ? -b1.d[j] : b1.d[j];
      exp = (abs_a > abs_b) ? 
	    (((unsigned long long)(0xFFFFFFFF) << 32) 
	     | (unsigned long long)(0xFFFFFFFF)) : 0;
      if (exp != d.ull[j]) abort();
    }
  return 0;
}

int test_spu_testsv()
{
  int i, j;
  unsigned long long exp;
  struct _samples {
    unsigned long long v;
    unsigned int sv;
 } samples[] = {
  {0x0000000000000000ULL, SPU_SV_POS_ZERO},
  {0x8000000000000000ULL, SPU_SV_NEG_ZERO},
  {0x0000000000000001ULL, SPU_SV_POS_DENORM},
  {0x0000000080000000ULL, SPU_SV_POS_DENORM},
  {0x0000000100000000ULL, SPU_SV_POS_DENORM},
  {0x0008000000000000ULL, SPU_SV_POS_DENORM},
  {0x000FFFFFFFFFFFFFULL, SPU_SV_POS_DENORM},
  {0x00000000FFF00000ULL, SPU_SV_POS_DENORM},
  {0x8000000000000001ULL, SPU_SV_NEG_DENORM},
  {0x8000000080000000ULL, SPU_SV_NEG_DENORM},
  {0x8000000100000000ULL, SPU_SV_NEG_DENORM},
  {0x8008000000000000ULL, SPU_SV_NEG_DENORM},
  {0x800FFFFFFFFFFFFFULL, SPU_SV_NEG_DENORM},
  {0x80000000FFF00000ULL, SPU_SV_NEG_DENORM},
  {0x0010000000000000ULL, 0},
  {0x0010000000000001ULL, 0},
  {0x3FF0000000000000ULL, 0},
  {0x3FF00000FFF00000ULL, 0},
  {0xBFF0000000000000ULL, 0},
  {0xBFF00000FFF00000ULL, 0},
  {0x7FE0000000000000ULL, 0},
  {0x7FEFFFFFFFFFFFFFULL, 0},
  {0x8010000000000000ULL, 0},
  {0x8010000000000001ULL, 0},
  {0xFFE0000000000000ULL, 0},
  {0xFFEFFFFFFFFFFFFFULL, 0},
  {0x7FF0000000000000ULL, SPU_SV_POS_INFINITY},
  {0xFFF0000000000000ULL, SPU_SV_NEG_INFINITY},
  {0x7FF0000000000001ULL, SPU_SV_NAN},
  {0x7FF0000080000000ULL, SPU_SV_NAN},
  {0x7FF0000100000000ULL, SPU_SV_NAN},
  {0x7FFFFFFFFFFFFFFFULL, SPU_SV_NAN},
  {0xFFF0000000000001ULL, SPU_SV_NAN},
  {0xFFF0000080000000ULL, SPU_SV_NAN},
  {0xFFF0000100000000ULL, SPU_SV_NAN},
  {0xFFFFFFFFFFFFFFFFULL, SPU_SV_NAN}
 };

  unsigned char cnt = sizeof(samples)/sizeof(struct _samples);
  int e0;
    for (e0=0; e0<cnt; e0++)
    {   
      a.ull[0] = samples[e0].v;
      a.d[1] = rand_double(-1, -4);

      d.vull = spu_testsv(a.vd, SPU_SV_NEG_DENORM);
      exp = (SPU_SV_NEG_DENORM & samples[e0].sv) ? 0xFFFFFFFFFFFFFFFFULL : 0ULL;
      if (exp != d.ull[0] || d.ull[1] != 0) abort();
    
      d.vull = spu_testsv(a.vd, SPU_SV_POS_DENORM);
      exp = (SPU_SV_POS_DENORM & samples[e0].sv) ? 0xFFFFFFFFFFFFFFFFULL : 0ULL;
      if (exp != d.ull[0] || d.ull[1] != 0) abort();
    
      d.vull = spu_testsv(a.vd, SPU_SV_NEG_ZERO);
      exp = (SPU_SV_NEG_ZERO & samples[e0].sv) ? 0xFFFFFFFFFFFFFFFFULL : 0ULL;
      if (exp != d.ull[0] || d.ull[1] != 0) abort();
    
      d.vull = spu_testsv(a.vd, SPU_SV_POS_ZERO);
      exp = (SPU_SV_POS_ZERO & samples[e0].sv) ? 0xFFFFFFFFFFFFFFFFULL : 0ULL;
      if (exp != d.ull[0] || d.ull[1] != 0) abort();
    
      d.vull = spu_testsv(a.vd, SPU_SV_NEG_INFINITY);
      exp = (SPU_SV_NEG_INFINITY & samples[e0].sv) ? 0xFFFFFFFFFFFFFFFFULL : 0ULL;
      if (exp != d.ull[0] || d.ull[1] != 0) abort();
    
      d.vull = spu_testsv(a.vd, SPU_SV_POS_INFINITY);
      exp = (SPU_SV_POS_INFINITY & samples[e0].sv) ? 0xFFFFFFFFFFFFFFFFULL : 0ULL;
      if (exp != d.ull[0] || d.ull[1] != 0) abort();
    
      d.vull = spu_testsv(a.vd, SPU_SV_NAN);
      exp = (SPU_SV_NAN & samples[e0].sv) ? 0xFFFFFFFFFFFFFFFFULL : 0ULL;
      if (exp != d.ull[0] || d.ull[1] != 0) abort();
   }  
  return 0;
}

int main()
{
  /* +0.0 and -0.0  */
  a0.d[0] = 0.0; a0.d[1] = -0.0; b0.d[0] = -0.0; b0.d[1] = 0.0;
  /* NaN  */
  a1.d[0] = 0.0/0.0; a1.d[1] = 0.0/-0.0; b1.d[0] = -0.0/0.0;  b1.d[1] = -0.0/-0.0;

  test_spu_cmpeq();
  test_spu_cmpabseq();
  test_spu_cmpgt();
  test_spu_cmpabsgt();
  test_spu_testsv();
  return 0;
}


