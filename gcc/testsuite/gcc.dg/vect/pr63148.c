
#include "tree-vect.h"

/* Extracted from MultiSource/Benchmarks/TSVC/tsc.inc
   From LLVM test-suite */

#define N 40

int dummy(double[N], double[N], double[N], double[N]);

double array[256*256] __attribute__((aligned(32)));

double x[N] __attribute__((aligned(32)));
double temp;
int temp_int;
struct GlobalData
{
  __attribute__((aligned(32))) double a[N];
  int pad1[3];
  __attribute__((aligned(32))) double b[N];
  int pad2[5];
  __attribute__((aligned(32))) double c[N];
  int pad3[7];
  __attribute__((aligned(32))) double d[N];
  int pad4[11];
} global_data;

__attribute__((aligned(32))) double * const a = global_data.a;
__attribute__((aligned(32))) double * const b = global_data.b;
__attribute__((aligned(32))) double * const c = global_data.c;
__attribute__((aligned(32))) double * const d = global_data.d;

void init(void);
void check(double *_a, double *_b);
int s221(void)
{
  int i;

  init();
  for (i = 1; i < N; i++)
    {
      a[i] += c[i] * d[i];
      b[i] = b[i - 1] + a[i] + d[i];
    }
  check(a, b);
  return 0;
}

int set1d(double arr[N], double value)
{
  int i;

  for (i = 0; i < N; i++) {
    arr[i] = value;
  }
  return 0;
}

void init(void)
{
  set1d(a, 1);
  set1d(b, 2);
  set1d(c, 3);
  set1d(d, 4);
}

void abort(void);

void check(double *_a, double *_b)
{
  int i;

  double suma = 0;
  double sumb = 0;
  for (i = 0; i < N; i++){
    suma += _a[i];
    sumb += _b[i];
  }
  if (suma != 508)
    abort();
  if (sumb != 13340.00)
    abort();
}

int main(int argc, char *argv[])
{
  check_vect ();
  s221();
  return 0;
}

