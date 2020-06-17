// PR c++/83058 - ICE on C++ code with negative array index: in
// warn_placement_new_too_small
// { dg-do compile }
// { dg-additional-options "-Wplacement-new -Wno-pedantic" }
// { dg-require-effective-target alloca }

#define SIZE_MAX   __SIZE_MAX__
#define DIFF_MAX   __PTRDIFF_MAX__
#define DIFF_MIN   (-DIFF_MAX - 1)

void* operator new (__SIZE_TYPE__ n, void *p) { return p; }
void* operator new[] (__SIZE_TYPE__ n, void *p) { return p; }

typedef int int32_t __attribute__((mode (__SI__)));

struct A { };

char carr[2];
int32_t iarr[2];

struct C0 { char i, carr[0]; };
struct I0 { int32_t i, iarr[0]; };
struct CX { char i, carr[]; };
struct IX { int32_t i, iarr[]; };

void test_single (C0 *pc, CX *qc, I0 *pi, IX *qi, int32_t n)
{
  new (&carr[DIFF_MIN]) A ();       // { dg-warning "placement new constructing an object of type .A. and size .1. in a region of type .char \\\[2]. and size .0." }
  new (&carr[-1]) A;                // { dg-warning "\\\[-Wplacement-new" }
  new (carr -1 ) A;                 // { dg-warning "\\\[-Wplacement-new" }
  new (&carr[0]) A;
  new (carr) A;
  new (&carr[1]) A;
  new (carr + 1) A;
  new (&carr[n]) A;
  new (carr + n) A;
  new (&carr[DIFF_MAX]) A;          // { dg-warning "\\\[-Wplacement-new" }
  new (carr + DIFF_MAX) A;          // { dg-warning "\\\[-Wplacement-new" }
  new (&carr[SIZE_MAX]) A;          // { dg-warning "\\\[-Wplacement-new" }
  new (carr + SIZE_MAX) A;          // { dg-warning "\\\[-Wplacement-new" }

  new (&pc->carr[DIFF_MIN]) A;      // { dg-warning "\\\[-Wplacement-new" }
  new (&pc->carr[-1]) A;            // { dg-warning "\\\[-Wplacement-new" }
  new (&pc->carr[0]) A;
  new (&pc->carr[9]) A;
  new (&pc->carr[n]) A;
  new (&pc->carr[DIFF_MAX]) A;      // { dg-warning "\\\[-Wplacement-new" }
  new (&pc->carr[SIZE_MAX]) A;      // { dg-warning "\\\[-Wplacement-new" }

  {
    /* The highest index at which a single A can be constructed.  */
    enum { MAX = DIFF_MAX - sizeof *pc - sizeof (A) };
    new (&pc->carr[MAX]) A;
    new (&pc->carr[MAX + 1]) A;     // { dg-warning "\\\[-Wplacement-new" }
  }

  new (&qc->carr[DIFF_MIN]) A;      // { dg-warning "\\\[-Wplacement-new" }
  new (&qc->carr[-1]) A;            // { dg-warning "\\\[-Wplacement-new" }
  new (&qc->carr[0]) A;
  new (&qc->carr[9]) A;
  new (&qc->carr[n]) A;
  new (&qc->carr[DIFF_MAX]) A;      // { dg-warning "\\\[-Wplacement-new" }
  new (&qc->carr[SIZE_MAX]) A;      // { dg-warning "\\\[-Wplacement-new" }

  {
    /* The highest index at which a single A can be constructed.  */
    enum { MAX = DIFF_MAX - sizeof *qc - sizeof (A) };
    new (&qc->carr[MAX]) A;
    new (&qc->carr[MAX + 1]) A;     // { dg-warning "\\\[-Wplacement-new" }
  }

  new (&pi->iarr[DIFF_MIN]) A;      // { dg-warning "\\\[-Wplacement-new" }
  new (&pi->iarr[-1]) A;            // { dg-warning "\\\[-Wplacement-new" }
  new (&pi->iarr[0]) A;
  new (&pi->iarr[9]) A;
  new (&pi->iarr[n]) A;
  new (&pi->iarr[DIFF_MAX]) A;      // { dg-warning "\\\[-Wplacement-new" }
  new (&pi->iarr[SIZE_MAX]) A;      // { dg-warning "\\\[-Wplacement-new" }

  {
    enum { MAX = (DIFF_MAX - sizeof *pi) / sizeof *pi->iarr };
    new (&pi->iarr[MAX]) A;
    new (&pi->iarr[MAX + 1]) A;     // { dg-warning "\\\[-Wplacement-new" }
  }

  new (&qi->iarr[DIFF_MIN]) A;      // { dg-warning "\\\[-Wplacement-new" }
  new (&qi->iarr[-1]) A;            // { dg-warning "\\\[-Wplacement-new" }
  new (&qi->iarr[0]) A;
  new (&qi->iarr[9]) A;
  new (&qi->iarr[n]) A;
  new (&qi->iarr[DIFF_MAX]) A;      // { dg-warning "\\\[-Wplacement-new" }
  new (&qi->iarr[SIZE_MAX]) A;      // { dg-warning "\\\[-Wplacement-new" }

  {
    enum { MAX = (DIFF_MAX - sizeof *qi) / sizeof *qi->iarr };
    new (&qi->iarr[MAX]) A;
    new (&qi->iarr[MAX + 1]) A;     // { dg-warning "\\\[-Wplacement-new" }
  }

  new (&iarr[DIFF_MIN]) A;          // { dg-warning "\\\[-Wplacement-new" }
  new (&iarr[-1]) A;                // { dg-warning "\\\[-Wplacement-new" }
  new (&iarr[1]) A;
  new (&iarr[n]) A;
  new (&iarr[DIFF_MAX]) A;          // { dg-warning "\\\[-Wplacement-new" }
  new (&iarr[SIZE_MAX]) A;          // { dg-warning "\\\[-Wplacement-new" }
}

void test_array_1 (C0 *pc, CX *qc, I0 *pi, IX *qi)
{
  enum { N = 1 };

  new (&carr[DIFF_MIN]) A[N];       // { dg-warning "placement new constructing an object of type .A \\\[\[0-9\]+]. and size .\[0-9\]+. in a region of type .char \\\[2]. and size .0." }
  new (&carr[-1]) A[N];             // { dg-warning "\\\[-Wplacement-new" }
  new (&carr[DIFF_MAX]) A[N];       // { dg-warning "\\\[-Wplacement-new" }
  new (&carr[SIZE_MAX]) A[N];       // { dg-warning "\\\[-Wplacement-new" }

  new (&pc->carr[DIFF_MIN]) A[N];   // { dg-warning "\\\[-Wplacement-new" }
  new (&pc->carr[-1]) A[N];         // { dg-warning "\\\[-Wplacement-new" }
  new (&pc->carr[DIFF_MAX]) A[N];   // { dg-warning "\\\[-Wplacement-new" }
  new (&pc->carr[SIZE_MAX]) A[N];   // { dg-warning "\\\[-Wplacement-new" }

  {
    enum { MAX = DIFF_MAX - sizeof *pc - sizeof (A[N]) };
    new (&pc->carr[MAX]) A[N];
    new (&pc->carr[MAX + 1]) A[N];  // { dg-warning "\\\[-Wplacement-new" }
  }

  new (&qc->carr[DIFF_MIN]) A[N];   // { dg-warning "\\\[-Wplacement-new" }
  new (&qc->carr[-1]) A[N];         // { dg-warning "\\\[-Wplacement-new" }
  new (&qc->carr[DIFF_MAX]) A[N];   // { dg-warning "\\\[-Wplacement-new" }
  new (&qc->carr[SIZE_MAX]) A[N];   // { dg-warning "\\\[-Wplacement-new" }

  {
    enum { MAX = DIFF_MAX - sizeof *qc - sizeof (A[N]) };
    new (&qc->carr[MAX]) A[N];
    new (&qc->carr[MAX + 1]) A[N];  // { dg-warning "\\\[-Wplacement-new" }
  }

  new (&pi->iarr[DIFF_MIN]) A[N];   // { dg-warning "\\\[-Wplacement-new" }
  new (&pi->iarr[-1]) A[N];         // { dg-warning "\\\[-Wplacement-new" }
  new (&pi->iarr[DIFF_MAX]) A[N];   // { dg-warning "\\\[-Wplacement-new" }
  new (&pi->iarr[SIZE_MAX]) A[N];   // { dg-warning "\\\[-Wplacement-new" }

  {
    enum { MAX = (DIFF_MAX - sizeof *pi) / sizeof *pi->iarr };
    new (&pi->iarr[MAX]) A[N];
    new (&pi->iarr[MAX + 1]) A[N];  // { dg-warning "\\\[-Wplacement-new" }
  }

  new (&qi->iarr[DIFF_MIN]) A[N];   // { dg-warning "\\\[-Wplacement-new" }
  new (&qi->iarr[-1]) A[N];         // { dg-warning "\\\[-Wplacement-new" }
  new (&qi->iarr[DIFF_MAX]) A[N];   // { dg-warning "\\\[-Wplacement-new" }
  new (&qi->iarr[SIZE_MAX]) A[N];   // { dg-warning "\\\[-Wplacement-new" }

  {
    enum { MAX = (DIFF_MAX - sizeof *qi) / sizeof *qi->iarr };
    new (&qi->iarr[MAX]) A[N];
    new (&qi->iarr[MAX + 1]) A[N];  // { dg-warning "\\\[-Wplacement-new" }
  }

  new (&iarr[DIFF_MIN]) A[N];       // { dg-warning "\\\[-Wplacement-new" }
  new (&iarr[-1]) A[N];             // { dg-warning "\\\[-Wplacement-new" }
  new (&iarr[DIFF_MAX]) A[N];       // { dg-warning "\\\[-Wplacement-new" }
  new (&iarr[SIZE_MAX]) A[N];       // { dg-warning "\\\[-Wplacement-new" }
}


void test_array_3 (C0 *pc, CX *qc, I0 *pi, IX *qi)
{
  enum { N = 3 };

  new (&carr[DIFF_MIN]) A[N];       // { dg-warning "placement new constructing an object of type .A \\\[\[0-9\]+]. and size .\[0-9\]+. in a region of type .char \\\[2]. and size .0." }
  new (&carr[-1]) A[N];             // { dg-warning "\\\[-Wplacement-new" }
  new (&carr[DIFF_MAX]) A[N];       // { dg-warning "\\\[-Wplacement-new" }
  new (&carr[SIZE_MAX]) A[N];       // { dg-warning "\\\[-Wplacement-new" }

  new (&pc->carr[DIFF_MIN]) A[N];   // { dg-warning "\\\[-Wplacement-new" }
  new (&pc->carr[-1]) A[N];         // { dg-warning "\\\[-Wplacement-new" }
  new (&pc->carr[DIFF_MAX]) A[N];   // { dg-warning "\\\[-Wplacement-new" }
  new (&pc->carr[SIZE_MAX]) A[N];   // { dg-warning "\\\[-Wplacement-new" }

  {
    enum { MAX = DIFF_MAX - sizeof *pc - sizeof (A[N]) };
    new (&pc->carr[MAX]) A[N];
    new (&pc->carr[MAX + 1]) A[N];  // { dg-warning "\\\[-Wplacement-new" }
  }

  new (&qc->carr[DIFF_MIN]) A[N];   // { dg-warning "\\\[-Wplacement-new" }
  new (&qc->carr[-1]) A[N];         // { dg-warning "\\\[-Wplacement-new" }
  new (&qc->carr[DIFF_MAX]) A[N];   // { dg-warning "\\\[-Wplacement-new" }
  new (&qc->carr[SIZE_MAX]) A[N];   // { dg-warning "\\\[-Wplacement-new" }

  {
    enum { MAX = DIFF_MAX - sizeof *qc - sizeof (A[N]) };
    new (&qc->carr[MAX]) A[N];
    new (&qc->carr[MAX + 1]) A[N];  // { dg-warning "\\\[-Wplacement-new" }
  }

  new (&pi->iarr[DIFF_MIN]) A[N];   // { dg-warning "\\\[-Wplacement-new" }
  new (&pi->iarr[-1]) A[N];         // { dg-warning "\\\[-Wplacement-new" }
  new (&pi->iarr[DIFF_MAX]) A[N];   // { dg-warning "\\\[-Wplacement-new" }
  new (&pi->iarr[SIZE_MAX]) A[N];   // { dg-warning "\\\[-Wplacement-new" }

  {
    enum { MAX = (DIFF_MAX - sizeof *pi) / sizeof *pi->iarr };
    new (&pi->iarr[MAX]) A[N];
    new (&pi->iarr[MAX + 1]) A[N];  // { dg-warning "\\\[-Wplacement-new" }
  }

  new (&qi->iarr[DIFF_MIN]) A[N];   // { dg-warning "\\\[-Wplacement-new" }
  new (&qi->iarr[-1]) A[N];         // { dg-warning "\\\[-Wplacement-new" }
  new (&qi->iarr[DIFF_MAX]) A[N];   // { dg-warning "\\\[-Wplacement-new" }
  new (&qi->iarr[SIZE_MAX]) A[N];   // { dg-warning "\\\[-Wplacement-new" }

  {
    enum { MAX = (DIFF_MAX - sizeof *qi) / sizeof *qi->iarr };
    new (&qi->iarr[MAX]) A[N];
    new (&qi->iarr[MAX + 1]) A[N];  // { dg-warning "\\\[-Wplacement-new" }
  }

  new (&iarr[DIFF_MIN]) A[N];       // { dg-warning "\\\[-Wplacement-new" }
  new (&iarr[-1]) A[N];             // { dg-warning "\\\[-Wplacement-new" }
  new (&iarr[DIFF_MAX]) A[N];       // { dg-warning "\\\[-Wplacement-new" }
  new (&iarr[SIZE_MAX]) A[N];       // { dg-warning "\\\[-Wplacement-new" }
}


void test_vla (unsigned n)
{
  char cvla[n];

  new (&cvla[DIFF_MIN]) A;          // { dg-warning "placement new constructing an object of type .A. and size .1. in a region of type .char \\\[n]. and size .0." }
  new (&cvla[-1]) A;                // { dg-warning "\\\[-Wplacement-new" }
  new (cvla -1) A;                  // { dg-warning "\\\[-Wplacement-new" }
  new (&cvla[0]) A;
  new (&cvla[9]) A;
  new (&cvla[n - 1]) A;
  new (cvla + n - 1) A;
  new (&cvla[DIFF_MAX - 1]) A;
  new (&cvla[DIFF_MAX]) A;          // { dg-warning "\\\[-Wplacement-new" }
  new (cvla + DIFF_MAX) A;          // { dg-warning "\\\[-Wplacement-new" }
  new (&cvla[SIZE_MAX]) A;          // { dg-warning "\\\[-Wplacement-new" }
  new (cvla + SIZE_MAX) A;          // { dg-warning "\\\[-Wplacement-new" }
}
