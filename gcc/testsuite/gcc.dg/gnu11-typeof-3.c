/* Test GNU extensions __typeof__ and __typeof_unqual__.  Valid code.  */
/* { dg-do run } */
/* { dg-options "-std=gnu11" } */

int i;
extern __typeof__ (i) i;
extern __typeof (int) i;
extern __typeof_unqual__ (i) i;
extern __typeof_unqual (int) i;

volatile int vi;
extern __typeof__ (volatile int) vi;
extern __typeof (vi) vi;

extern __typeof_unqual__ (volatile int) i;
extern __typeof_unqual__ (vi) i;
extern __typeof__ ((const int) vi) i;
extern __typeof ((volatile int) vi) i;

const int ci;
extern __typeof (const int) ci;
extern __typeof (ci) ci;

extern __typeof_unqual (const int) i;
extern __typeof_unqual (ci) i;
extern __typeof__ ((const int) ci) i;
extern __typeof__ (+ci) i;
extern __typeof (0, ci) i;
extern __typeof__ (1 ? ci : ci) i;
extern __typeof (0) i;

const int fci (void);
extern __typeof__ (fci ()) i;

_Atomic int ai;
extern __typeof (_Atomic int) ai;
extern __typeof__ (_Atomic (int)) ai;
extern __typeof (ai) ai;

extern __typeof_unqual__ (_Atomic int) i;
extern __typeof_unqual (_Atomic (int)) i;
extern __typeof_unqual__ (ai) i;
extern __typeof (+ai) i;
extern __typeof__ ((_Atomic int) ai) i;
extern __typeof__ (0, ai) i;
extern __typeof (1 ? ai : ai) i;

_Atomic int fai (void);
extern __typeof__ (fai ()) i;

_Atomic const volatile int acvi;
extern __typeof (int volatile const _Atomic) acvi;
extern __typeof (acvi) acvi;
extern const _Atomic volatile __typeof (acvi) acvi;
extern _Atomic volatile __typeof__ (ci) acvi;
extern _Atomic const __typeof (vi) acvi;
extern const __typeof__ (ai) volatile acvi;

extern __typeof_unqual (acvi) i;
extern __typeof_unqual__ (__typeof (acvi)) i;
extern __typeof_unqual (_Atomic __typeof_unqual__ (acvi)) i;

extern _Atomic __typeof_unqual (acvi) ai;

char c;
volatile char vc;
volatile char *pvc;
volatile char *const cpvc;
const char *pcc;
const char *volatile vpcc;
__typeof (*vpcc) cc;

extern __typeof__ (*cpvc) vc;
extern __typeof_unqual (*cpvc) c;
extern __typeof_unqual__ (cpvc) pvc;
extern __typeof_unqual__ (vpcc) pcc;
extern const char cc;

extern __typeof (++vi) i;
extern __typeof (++ai) i;
extern __typeof__ (--vi) i;
extern __typeof (--ai) i;
extern __typeof__ (vi++) i;
extern __typeof__ (ai++) i;
extern __typeof (vi--) i;
extern __typeof__ (ai--) i;

_Bool b;
volatile _Bool vb;
_Atomic _Bool ab;
extern __typeof__ (++vb) b;
extern __typeof__ (++ab) b;
extern __typeof (--vb) b;
extern __typeof__ (--ab) b;
extern __typeof (vb++) b;
extern __typeof (ab++) b;
extern __typeof__ (vb--) b;
extern __typeof (ab--) b;

extern __typeof__ (vc = 1) c;
extern __typeof__ (vpcc = 0) pcc;
extern __typeof (ai *= 2) i;

int s = sizeof (__typeof__ (int (*)[++i]));

void *vp;

extern void abort (void);
extern void exit (int);

extern int only_used_in_typeof;

static int not_defined (void);

__typeof (i)
main (__typeof (*vp))
{
  volatile __typeof__ (only_used_in_typeof) ii = 2;
  if (ii != 2)
    abort ();
  const __typeof__ (not_defined ()) jj = 3;
  if (jj != 3)
    abort ();
  unsigned int u = 1;
  __typeof__ (u) u2 = 0;
  __typeof (int (*)[++u2]) p = 0;
  if (u2 != 1)
    abort ();
  if (sizeof (*p) != sizeof (int))
    abort ();
  __typeof_unqual (int (*)[++u2]) q = 0;
  if (u2 != 2)
    abort ();
  if (sizeof (*q) != 2 * sizeof (int))
    abort ();
  if (sizeof (*p) != sizeof (int))
    abort ();
  __typeof (++u2) u3 = 1;
  if (u2 != u + u3)
    abort ();
  __typeof_unqual__ (++u2) u4 = 2;
  if (u2 != u4)
    abort ();
  u = sizeof (__typeof__ (int (*)[++u2]));
  if (u2 != 2)
    abort ();
  u = sizeof (__typeof_unqual (int (*)[++u2]));
  if (u2 != 2)
    abort ();
  __typeof ((int (*)[++u2]) 0) q2;
  if (u2 != 3)
    abort ();
  __typeof ((void) 0, (int (*)[++u2]) 0) q3;
  if (u2 != 4)
    abort ();
  __typeof__ ((int (*)[++u2]) 0, 0) q4;
  if (u2 != 4)
    abort ();
  __typeof_unqual ((int (*)[++u2]) 0) q5;
  if (u2 != 5)
    abort ();
  __typeof_unqual__ ((void) 0, (int (*)[++u2]) 0) q6;
  if (u2 != 6)
    abort ();
  __typeof_unqual__ ((int (*)[++u2]) 0, 0) q7;
  if (u2 != 6)
    abort ();
  int a1[6], a2[6];
  int (*pa)[u2] = &a1;
  __typeof (pa = &a2) pp;
  if (pa != &a2)
    abort ();
  __typeof_unqual (pa = &a1) pp2;
  if (pa != &a1)
    abort ();
  exit (0);
}
