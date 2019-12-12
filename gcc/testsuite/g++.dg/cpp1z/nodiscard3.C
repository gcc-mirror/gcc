/* nodiscard attribute tests, adapted from gcc.dg/attr-warn-unused-result.c.  */
/* { dg-do compile { target c++17 } } */
/* { dg-options "-O -ftrack-macro-expansion=0" } */

#define WUR [[nodiscard]]
#define WURAI [[nodiscard, gnu::always_inline]] inline
enum [[nodiscard]] E { e };
typedef E (*fnt) (void);

typedef struct { long i; } A;
typedef struct { long i; long j; } B;
typedef struct { char big[1024]; fnt fn; } C;
struct [[nodiscard]] D { int i; D(); ~D(); };

WUR E check1 (void);
WUR void check2 (void); /* { dg-warning "10:.nodiscard." } */
WUR int foo;		/* { dg-warning "9:.nodiscard." } */
int bar (void);
WURAI E check3 (void) { return (E)bar (); }
WUR A check4 (void);
WUR B check5 (void);
WUR C check6 (void);
A bar7 (void);
B bar8 (void);
C bar9 (void);
WURAI A check7 (void) { return bar7 (); }
WURAI B check8 (void) { return bar8 (); }
WURAI C check9 (void) { return bar9 (); }
/* This is useful for checking whether return value of statement
   expressions (returning int in this case) is used.  */
WURAI int check_int_result (int res) { return res; }
#define GU(v) ({ int e = 0; (v) = bar (); if ((v) < 23) e = 14; e; })
fnt fnptr;
WUR E check10 (void);
int baz (void);
WURAI E check11 (void) { return (E)baz (); }
int k;

D check12();

void
test (void)
{
  int i = 0, j;
  const fnt pcheck1 = check1;
  const fnt pcheck3 = check3;
  A a;
  B b;
  C c;
  D d;
  if (check1 ())
    return;
  i += check1 ();
  i += ({ check1 (); });
  check1 ();		/* { dg-warning "nodiscard" } */
  (void) check1 ();
  check1 (), bar ();	/* { dg-warning "nodiscard" } */
  check2 ();
  (void) check2 ();
  check2 (), bar ();
  if (check3 ())
    return;
  i += check3 ();
  i += ({ check3 (); });
  check3 ();		/* { dg-warning "nodiscard" } */
  (void) check3 ();
  check3 (), bar ();	/* { dg-warning "nodiscard" } */
  a = check4 ();
  if (a.i)
    return;
  if (check4 ().i)
    return;
  if (({ check4 (); }).i)
    return;
  check4 ();		/* { dg-warning "nodiscard" } */
  (void) check4 ();
  check4 (), bar ();	/* { dg-warning "nodiscard" } */
  b = check5 ();
  if (b.i + b.j)
    return;
  if (check5 ().j)
    return;
  if (({ check5 (); }).j)
    return;
  check5 ();		/* { dg-warning "nodiscard" } */
  (void) check5 ();
  check5 (), bar ();	/* { dg-warning "nodiscard" } */
  c = check6 ();
  if (c.big[12] + c.big[29])
    return;
  if (check6 ().big[27])
    return;
  if (({ check6 (); }).big[0])
    return;
  check6 ();		/* { dg-warning "nodiscard" } */
  (void) check6 ();
  check6 (), bar ();	/* { dg-warning "nodiscard" } */
  a = check7 ();
  if (a.i)
    return;
  if (check7 ().i)
    return;
  if (({ check7 (); }).i)
    return;
  check7 ();		/* { dg-warning "nodiscard" } */
  (void) check7 ();
  check7 (), bar ();	/* { dg-warning "nodiscard" } */
  b = check8 ();
  if (b.i + b.j)
    return;
  if (check8 ().j)
    return;
  if (({ check8 (); }).j)
    return;
  check8 ();		/* { dg-warning "nodiscard" } */
  (void) check8 ();
  check8 (), bar ();	/* { dg-warning "nodiscard" } */
  c = check9 ();
  if (c.big[12] + c.big[29])
    return;
  if (check9 ().big[27])
    return;
  if (({ check9 (); }).big[0])
    return;
  check9 ();		/* { dg-warning "nodiscard" } */
  (void) check9 ();
  check9 (), bar ();	/* { dg-warning "nodiscard" } */
  if (check_int_result (GU (j)))
    return;
  i += check_int_result (GU (j));
  i += ({ check_int_result (GU (j)); });
  check_int_result (GU (j)); /* { dg-warning "nodiscard" } */
  (void) check_int_result (GU (j));
  check_int_result (GU (j)), bar (); /* { dg-warning "nodiscard" } */
  if (fnptr ())
    return;
  i += fnptr ();
  i += ({ fnptr (); });
  fnptr ();		/* { dg-warning "nodiscard" } */
  (void) fnptr ();
  fnptr (), bar ();	/* { dg-warning "nodiscard" } */
  fnptr = check1;
  if (fnptr ())
    return;
  i += fnptr ();
  i += ({ fnptr (); });
  fnptr ();		/* { dg-warning "nodiscard" } */
  (void) fnptr ();
  fnptr (), bar ();	/* { dg-warning "nodiscard" } */
  fnptr = check3;
  if (fnptr ())
    return;
  i += fnptr ();
  i += ({ fnptr (); });
  fnptr ();		/* { dg-warning "nodiscard" } */
  (void) fnptr ();
  fnptr (), bar ();	/* { dg-warning "nodiscard" } */
  if (bar9 ().fn ())
    return;
  i += bar9 ().fn ();
  i += ({ bar9 ().fn (); });
  bar9 ().fn ();	/* { dg-warning "nodiscard" } */
  (void) bar9 ().fn ();
  bar9 ().fn (), bar (); /* { dg-warning "nodiscard" } */
  if ((k ? check1 : check10) ())
    return;
  i += (k ? check1 : check10) ();
  i += ({ (k ? check1 : check10) (); });
  (k ? check1 : check10) (); /* { dg-warning "nodiscard" } */
  (void) (k ? check1 : check10) ();
  (k ? check1 : check10) (), bar (); /* { dg-warning "nodiscard" } */
  if ((k ? check3 : check11) ())
    return;
  i += (k ? check3 : check11) ();
  i += ({ (k ? check3 : check11) (); });
  (k ? check3 : check11) (); /* { dg-warning "nodiscard" } */
  (void) (k ? check3 : check11) ();
  (k ? check3 : check11) (), bar (); /* { dg-warning "nodiscard" } */
  if (pcheck1 ())
    return;
  i += pcheck1 ();
  i += ({ pcheck1 (); });
  pcheck1 ();		/* { dg-warning "nodiscard" } */
  (void) pcheck1 ();
  pcheck1 (), bar ();	/* { dg-warning "nodiscard" } */
  if (pcheck3 ())
    return;
  i += pcheck3 ();
  i += ({ pcheck3 (); });
  pcheck3 ();		/* { dg-warning "nodiscard" } */
  (void) pcheck3 ();
  pcheck3 (), bar ();	/* { dg-warning "nodiscard" } */
  d = check12 ();
  if (d.i)
    return;
  if (check12 ().i)
    return;
  if (({ check12 (); }).i)
    return;
  check12 ();		/* { dg-warning "nodiscard" } */
  (void) check12 ();
  check12 (), bar ();	/* { dg-warning "nodiscard" } */
}
