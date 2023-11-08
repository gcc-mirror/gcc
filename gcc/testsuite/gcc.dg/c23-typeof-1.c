/* Test C23 typeof and typeof_unqual.  Valid code.  */
/* { dg-do run } */
/* { dg-options "-std=c23 -pedantic-errors" } */

int i;
extern typeof (i) i;
extern typeof (int) i;
extern typeof_unqual (i) i;
extern typeof_unqual (int) i;

volatile int vi;
extern typeof (volatile int) vi;
extern typeof (vi) vi;

extern typeof_unqual (volatile int) i;
extern typeof_unqual (vi) i;
extern typeof ((const int) vi) i;
extern typeof ((volatile int) vi) i;

const int ci;
extern typeof (const int) ci;
extern typeof (ci) ci;

extern typeof_unqual (const int) i;
extern typeof_unqual (ci) i;
extern typeof ((const int) ci) i;
extern typeof (+ci) i;
extern typeof (0, ci) i;
extern typeof (1 ? ci : ci) i;
extern typeof (0) i;

const int fci (void);
extern typeof (fci ()) i;

_Atomic int ai;
extern typeof (_Atomic int) ai;
extern typeof (_Atomic (int)) ai;
extern typeof (ai) ai;

extern typeof_unqual (_Atomic int) i;
extern typeof_unqual (_Atomic (int)) i;
extern typeof_unqual (ai) i;
extern typeof (+ai) i;
extern typeof ((_Atomic int) ai) i;
extern typeof (0, ai) i;
extern typeof (1 ? ai : ai) i;

_Atomic int fai (void);
extern typeof (fai ()) i;

_Atomic const volatile int acvi;
extern typeof (int volatile const _Atomic) acvi;
extern typeof (acvi) acvi;
extern const _Atomic volatile typeof (acvi) acvi;
extern _Atomic volatile typeof (ci) acvi;
extern _Atomic const typeof (vi) acvi;
extern const typeof (ai) volatile acvi;

extern typeof_unqual (acvi) i;
extern typeof_unqual (typeof (acvi)) i;
extern typeof_unqual (_Atomic typeof_unqual (acvi)) i;

extern _Atomic typeof_unqual (acvi) ai;

char c;
volatile char vc;
volatile char *pvc;
volatile char *const cpvc;
const char *pcc;
const char *volatile vpcc;
typeof (*vpcc) cc;

extern typeof (*cpvc) vc;
extern typeof_unqual (*cpvc) c;
extern typeof_unqual (cpvc) pvc;
extern typeof_unqual (vpcc) pcc;
extern const char cc;

extern typeof (++vi) i;
extern typeof (++ai) i;
extern typeof (--vi) i;
extern typeof (--ai) i;
extern typeof (vi++) i;
extern typeof (ai++) i;
extern typeof (vi--) i;
extern typeof (ai--) i;

bool b;
volatile bool vb;
_Atomic bool ab;
extern typeof (++vb) b;
extern typeof (++ab) b;
extern typeof (--vb) b;
extern typeof (--ab) b;
extern typeof (vb++) b;
extern typeof (ab++) b;
extern typeof (vb--) b;
extern typeof (ab--) b;

extern typeof (vc = 1) c;
extern typeof (vpcc = 0) pcc;
extern typeof (ai *= 2) i;

int s = sizeof (typeof (int (*)[++i]));

void *vp;

/* The non-returning property of a function is not part of the type given by
   ISO C typeof.  */
_Noreturn void nf1 (void);
[[noreturn]] void nf2 (void);
void fg (void) {}
typeof (&nf1) pnf1 = fg;
typeof (&nf2) pnf2 = fg;
extern void (*pnf1) (void);
extern void (*pnf2) (void);
extern typeof (nf1) *pnf1;
extern typeof (nf1) *pnf2;
extern typeof (nf2) *pnf1;
extern typeof (nf2) *pnf2;
typeof (*&nf1) fg2, fg2a, fg2b;
typeof (*&nf2) fg3, fg3a, fg3b;
typeof (nf1) fg4, fg4a, fg4b;
typeof (nf2) fg5, fg5a, fg5b;

extern void abort (void);
extern void exit (int);

void fg2 (void) {}
_Noreturn void fg2a (void) { abort (); }
[[noreturn]] void fg2b (void) { abort (); }
void fg3 (void) {}
_Noreturn void fg3a (void) { abort (); }
[[noreturn]] void fg3b (void) { abort (); }
void fg4 (void) {}
_Noreturn void fg4a (void) { abort (); }
[[noreturn]] void fg4b (void) { abort (); }
void fg5 (void) {}
_Noreturn void fg5a (void) { abort (); }
[[noreturn]] void fg5b (void) { abort (); }

extern int only_used_in_typeof;

static int not_defined (void);

typeof (i)
main (typeof (*vp))
{
  volatile typeof (only_used_in_typeof) ii = 2;
  if (ii != 2)
    abort ();
  const typeof (not_defined ()) jj = 3;
  if (jj != 3)
    abort ();
  unsigned int u = 1;
  typeof (u) u2 = 0;
  typeof (int (*)[++u2]) p = 0;
  if (u2 != 1)
    abort ();
  if (sizeof (*p) != sizeof (int))
    abort ();
  typeof_unqual (int (*)[++u2]) q = 0;
  if (u2 != 2)
    abort ();
  if (sizeof (*q) != 2 * sizeof (int))
    abort ();
  if (sizeof (*p) != sizeof (int))
    abort ();
  typeof (++u2) u3 = 1;
  if (u2 != u + u3)
    abort ();
  typeof_unqual (++u2) u4 = 2;
  if (u2 != u4)
    abort ();
  u = sizeof (typeof (int (*)[++u2]));
  if (u2 != 2)
    abort ();
  u = sizeof (typeof_unqual (int (*)[++u2]));
  if (u2 != 2)
    abort ();
  typeof ((int (*)[++u2]) 0) q2;
  if (u2 != 3)
    abort ();
  typeof ((void) 0, (int (*)[++u2]) 0) q3;
  if (u2 != 4)
    abort ();
  typeof ((int (*)[++u2]) 0, 0) q4;
  if (u2 != 4)
    abort ();
  typeof_unqual ((int (*)[++u2]) 0) q5;
  if (u2 != 5)
    abort ();
  typeof_unqual ((void) 0, (int (*)[++u2]) 0) q6;
  if (u2 != 6)
    abort ();
  typeof_unqual ((int (*)[++u2]) 0, 0) q7;
  if (u2 != 6)
    abort ();
  int a1[6], a2[6];
  int (*pa)[u2] = &a1;
  typeof (pa = &a2) pp;
  if (pa != &a2)
    abort ();
  typeof_unqual (pa = &a1) pp2;
  if (pa != &a1)
    abort ();
  exit (0);
}
