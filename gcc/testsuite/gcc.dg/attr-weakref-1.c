// { dg-do run }
// { dg-require-weak "" }
// On darwin, we use attr-weakref-1-darwin.c.

// This test requires support for undefined weak symbols.  This support
// is not available on the following targets.  The test is skipped rather than
// xfailed to suppress the warning that would otherwise arise.
// { dg-skip-if "" { "hppa*-*-hpux*" "*-*-aix*" "nvptx-*-*" } "*" { "" } }

// For kernel modules and static RTPs, the loader treats undefined weak
// symbols in the same way as undefined strong symbols.  The test
// therefore fails to load, so skip it.
// { dg-skip-if "" { "*-*-vxworks*" && nonpic } "*" { "-non-static" } }
// { dg-options "-O2" }
// { dg-additional-options "-Wl,-undefined,dynamic_lookup" { target *-*-darwin* } }
// { dg-additional-options "-Wl,-flat_namespace" { target *-*-darwin[89]* } }
// { dg-additional-sources "attr-weakref-1a.c" }

// Copyright 2005 Free Software Foundation, Inc.
// Contributed by Alexandre Oliva <aoliva@redhat.com>

// Torture test for weakrefs.  The first letter of an identifier
// indicates whether/how it is defined; the second letter indicates
// whether it is part of a variable or function test; the number that
// follows is a test counter, and a letter that may follow enables
// multiple identifiers within the same test (e.g., multiple weakrefs
// or pointers to the same identifier).

// Identifiers starting with W are weakrefs; those with p are
// pointers; those with g are global definitions; those with l are
// local definitions; those with w are expected to be weak undefined
// in the symbol table; those with u are expected to be marked as
// non-weak undefined in the symbol table.

#include <stdlib.h>

#define USED __attribute__((used))

typedef int vtype;

extern vtype wv1;
static vtype Wv1a __attribute__((weakref ("wv1")));
static vtype *pv1a USED = &Wv1a;

vtype gv2;
static vtype Wv2a __attribute__((weakref ("gv2")));
static vtype *pv2a USED = &Wv2a;

static vtype lv3;
static vtype Wv3a __attribute__((weakref ("lv3")));
static vtype *pv3a USED = &Wv3a;

extern vtype uv4;
static vtype Wv4a __attribute__((weakref ("uv4")));
static vtype *pv4a USED = &Wv4a;
static vtype *pv4 USED = &uv4;

static vtype Wv5a __attribute__((weakref ("uv5")));
static vtype *pv5a USED = &Wv5a;
extern vtype uv5;
static vtype *pv5 USED = &uv5;

static vtype Wv6a __attribute__((weakref ("wv6")));
static vtype *pv6a USED = &Wv6a;
extern vtype wv6;

static vtype Wv7a __attribute__((weakref ("uv7")));
static vtype* USED fv7 (void) {
  return &Wv7a;
}
extern vtype uv7;
static vtype* USED fv7a (void) {
  return &uv7;
}

extern vtype uv8;
static vtype* USED fv8a (void) {
  return &uv8;
}
static vtype Wv8a __attribute__((weakref ("uv8")));
static vtype* USED fv8 (void) {
  return &Wv8a;
}

extern vtype wv9 __attribute__((weak));
static vtype Wv9a __attribute__((weakref ("wv9")));
static vtype *pv9a USED = &Wv9a;

static vtype Wv10a __attribute__((weakref ("Wv10b")));
static vtype Wv10b __attribute__((weakref ("Wv10c")));
static vtype Wv10c __attribute__((weakref ("Wv10d")));
static vtype Wv10d __attribute__((weakref ("wv10")));
extern vtype wv10;

extern vtype wv11;
static vtype Wv11d __attribute__((weakref ("wv11")));
static vtype Wv11c __attribute__((weakref ("Wv11d")));
static vtype Wv11b __attribute__((weakref ("Wv11c")));
static vtype Wv11a __attribute__((weakref ("Wv11b")));

static vtype Wv12 __attribute__((weakref ("wv12")));
extern vtype wv12 __attribute__((weak));

static vtype Wv13 __attribute__((weakref ("wv13")));
extern vtype wv13 __attribute__((weak));

static vtype Wv14a __attribute__((weakref ("wv14")));
static vtype Wv14b __attribute__((weakref ("wv14")));
extern vtype wv14 __attribute__((weak));

typedef void ftype(void);

extern ftype wf1;
static ftype Wf1a __attribute__((weakref ("wf1")));
static ftype *pf1a USED = &Wf1a;
static ftype Wf1c __attribute__((weakref));
extern ftype Wf1c __attribute__((alias ("wf1")));
static ftype *pf1c USED = &Wf1c;

void gf2(void) {}
static ftype Wf2a __attribute__((weakref ("gf2")));
static ftype *pf2a USED = &Wf2a;

static void lf3(void) {}
static ftype Wf3a __attribute__((weakref ("lf3")));
static ftype *pf3a USED = &Wf3a;

extern ftype uf4;
static ftype Wf4a __attribute__((weakref ("uf4")));
static ftype *pf4a USED = &Wf4a;
static ftype *pf4 USED = &uf4;

static ftype Wf5a __attribute__((weakref ("uf5")));
static ftype *pf5a USED = &Wf5a;
extern ftype uf5;
static ftype *pf5 USED = &uf5;

static ftype Wf6a __attribute__((weakref ("wf6")));
static ftype *pf6a USED = &Wf6a;
extern ftype wf6;

static ftype Wf7a __attribute__((weakref ("uf7")));
static ftype* USED ff7 (void) {
  return &Wf7a;
}
extern ftype uf7;
static ftype* USED ff7a (void) {
  return &uf7;
}

extern ftype uf8;
static ftype* USED ff8a (void) {
  return &uf8;
}
static ftype Wf8a __attribute__((weakref ("uf8")));
static ftype* USED ff8 (void) {
  return &Wf8a;
}

extern ftype wf9 __attribute__((weak));
static ftype Wf9a __attribute__((weakref ("wf9")));
static ftype *pf9a USED = &Wf9a;

static ftype Wf10a __attribute__((weakref ("Wf10b")));
static ftype Wf10b __attribute__((weakref ("Wf10c")));
static ftype Wf10c __attribute__((weakref ("Wf10d")));
static ftype Wf10d __attribute__((weakref ("wf10")));
extern ftype wf10;

extern ftype wf11;
static ftype Wf11d __attribute__((weakref ("wf11")));
static ftype Wf11c __attribute__((weakref ("Wf11d")));
static ftype Wf11b __attribute__((weakref ("Wf11c")));
static ftype Wf11a __attribute__((weakref ("Wf11b")));

static ftype Wf12 __attribute__((weakref ("wf12")));
extern ftype wf12 __attribute__((weak));

static ftype Wf13 __attribute__((weakref ("wf13")));
extern ftype wf13 __attribute__((weak));

static ftype Wf14a __attribute__((weakref ("wf14")));
static ftype Wf14b __attribute__((weakref ("wf14")));
extern ftype wf14 __attribute__((weak));

#ifndef __APPLE__
#define chk(p) do { if (!p) abort (); } while (0)
#else
#define chk(p) /* */
#endif

int main () {
  chk (!pv1a);
  chk (pv2a);
  chk (pv3a);
  chk (pv4a);
  chk (pv4);
  chk (pv5a);
  chk (pv5);
  chk (!pv6a);
  chk (fv7 ());
  chk (fv7a ());
  chk (fv8 ());
  chk (fv8a ());
  chk (!pv9a);
  chk (!&Wv10a);
  chk (!&Wv11a);
  chk (!&Wv12);
  chk (!&wv12);
  chk (!&wv13);
  chk (!&Wv14a);

  chk (!pf1a);
  chk (!pf1c);
  chk (pf2a);
  chk (pf3a);
  chk (pf4a);
  chk (pf4);
  chk (pf5a);
  chk (pf5);
  chk (!pf6a);
  chk (ff7 ());
  chk (ff7a ());
  chk (ff8 ());
  chk (ff8a ());
  chk (!pf9a);
  chk (!&Wf10a);
  chk (!&Wf11a);
  chk (!&Wf12);
  chk (!&wf12);
  chk (!&wf13);
  chk (!&Wf14a);

  exit (0);
}
