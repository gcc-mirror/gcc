/* PR middle-end/98664 - inconsistent --Wfree-nonheap-object for inlined
   calls to system headers
   { dg-do compile }
   { dg-options "-O2 -Wall" } */

# 7 "Wfree-nonheap-object-5.h" 1 3

struct A0
{
  void *p;

  void f0 (void *q) { p = q; }
  void g0 (void) {
    __builtin_free (p);       // { dg-warning "\\\[-Wfree-nonheap-object" }
  }
};

struct A1
{
  void *p;

  void f0 (void *q) { p = q; }
  void f1 (void *q) { f0 (q); }

  void g0 (void) {
    __builtin_free (p);       // { dg-warning "\\\[-Wfree-nonheap-object" }
  }
  void g1 (void) { g0 (); }
};

struct A2
{
  void *p;

  void f0 (void *q) { p = q; }
  void f1 (void *q) { f0 (q); }
  void f2 (void *q) { f1 (q); }

  void g0 (void) {
    __builtin_free (p);       // { dg-warning "\\\[-Wfree-nonheap-object" }
  }
  void g1 (void) { g0 (); }
  void g2 (void) { g1 (); }
};

# 47 "Wfree-nonheap-object-5.C"

#define NOIPA __attribute__ ((noipa))

extern int array[];

/* Verify the warning is issued even for calls in a system header inlined
   into a function outside the header.  */

NOIPA void warn_g0 (struct A0 *p)
{
  int *q = array + 1;

  p->f0 (q);
  p->g0 ();
}

// { dg-message "inlined from 'void warn_g0\\(A0\\*\\)'" "" { target *-*-* } 0 }


/* Also verify the warning can be suppressed.  */

NOIPA void nowarn_g0 (struct A0 *p)
{
  int *q = array + 2;

  p->f0 (q);

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wfree-nonheap-object"
  p->g0 ();
#pragma GCC diagnostic pop
}


NOIPA void warn_g1 (struct A1 *p)
{
  int *q = array + 3;

  p->f1 (q);
  p->g1 ();
}

// { dg-message "inlined from 'void A1::g1\\(\\)'" "" { target *-*-* } 0 }
// { dg-message "inlined from 'void warn_g1\\(A1\\*\\)'" "" { target *-*-* } 0 }


NOIPA void nowarn_g1 (struct A2 *p)
{
  int *q = array + 4;

  p->f1 (q);

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wfree-nonheap-object"
  p->g1 ();
#pragma GCC diagnostic pop
}


NOIPA void warn_g2 (struct A2 *p)
{
  int *q = array + 5;

  p->f2 (q);
  p->g2 ();
}

// { dg-message "inlined from 'void A2::g1\\(\\)'" "" { target *-*-* } 0 }
// { dg-message "inlined from 'void A2::g2\\(\\)'" "" { target *-*-* } 0 }
// { dg-message "inlined from 'void warn_g2\\(A2\\*\\)'" "" { target *-*-* } 0 }


NOIPA void nowarn_g2 (struct A2 *p)
{
  int *q = array + 6;

  p->f2 (q);

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wfree-nonheap-object"
  p->g2 ();
#pragma GCC diagnostic pop
}
