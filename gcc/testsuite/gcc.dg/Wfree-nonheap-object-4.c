/* PR middle-end/98664 - inconsistent --Wfree-nonheap-object for inlined
   calls to system headers
   { dg-do compile }
   { dg-options "-O2 -Wall" } */

# 7 "Wfree-nonheap-object-4.h" 1 3

struct A
{
  void *p;
};

void f0 (struct A *p, void *q) { p->p = q; }
void f1 (struct A *p, void *q) { f0 (p, q); }
void f2 (struct A *p, void *q) { f1 (p, q); }

void g0 (struct A *p)
{
  __builtin_free (p->p);      // { dg-warning "\\\[-Wfree-nonheap-object" }
}

void g1 (struct A *p) { g0 (p); }
void g2 (struct A *p) { g1 (p); }

# 26 "Wfree-nonheap-object-4.c"

#define NOIPA __attribute__ ((noipa))

extern int array[];

/* Verify the warning is issued even for calls in a system header inlined
   into a function outside the header.  */

NOIPA void warn_g0 (struct A *p)
{
  int *q = array + 1;

  f0 (p, q);
  g0 (p);
}

// { dg-message "inlined from 'warn_g0'" "" { target *-*-* } 0 }


/* Also verify the warning can be suppressed.  */

NOIPA void nowarn_g0 (struct A *p)
{
  int *q = array + 2;

  f0 (p, q);

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wfree-nonheap-object"
  g0 (p);
#pragma GCC diagnostic pop
}


NOIPA void warn_g1 (struct A *p)
{
  int *q = array + 3;

  f1 (p, q);
  g1 (p);
}

// { dg-message "inlined from 'g1'" "" { target *-*-* } 0 }
// { dg-message "inlined from 'warn_g1'" "" { target *-*-* } 0 }


NOIPA void nowarn_g1 (struct A *p)
{
  int *q = array + 4;

  f1 (p, q);

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wfree-nonheap-object"
  g1 (p);
#pragma GCC diagnostic pop
}


NOIPA void warn_g2 (struct A *p)
{
  int *q = array + 5;

  f2 (p, q);
  g2 (p);
}

// { dg-message "inlined from 'g2'" "" { target *-*-* } 0 }
// { dg-message "inlined from 'warn_g2'" "" { target *-*-* } 0 }


NOIPA void nowarn_g2 (struct A *p)
{
  int *q = array + 6;

  f2 (p, q);

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wfree-nonheap-object"
  g2 (p);
#pragma GCC diagnostic pop
}
