/* { dg-do compile { target { powerpc*-*-* && lp64 } } } */
/* { dg-options "-O2 -mminimal-toc" } */

/* PR 39457 -- fix breakage because the compiler ran out of registers and
   wanted to stash a floating point value to the LR/CTR register.  */

/* -O2 -m64 -mminimal-toc */
typedef struct { void *s; } S;
typedef void (*T1) (void);
typedef void (*T2) (void *, void *, int, void *);
char *fn1 (const char *, ...);
void *fn2 (void);
int fn3 (char *, int);
int fn4 (const void *);
int fn5 (const void *);
long fn6 (void) __attribute__ ((__const__));
int fn7 (void *, void *, void *);
void *fn8 (void *, long);
void *fn9 (void *, long, const char *, ...);
void *fn10 (void *);
long fn11 (void) __attribute__ ((__const__));
long fn12 (void *, const char *, T1, T2, void *);
void *fn13 (void *);
long fn14 (void) __attribute__ ((__const__));
extern void *v1;
extern char *v2;
extern int v3;

void
foo (void *x, char *z)
{
  void *i1, *i2;
  int y;
  if (v1)
    return;
  v1 = fn9 (fn10 (fn2 ()), fn6 (), "x", 0., "y", 0., 0);
  y = 520 - (520 - fn4 (x)) / 2;
  fn9 (fn8 (v1, fn6 ()), fn6 (), "wig", fn8 (v1, fn14 ()), "x", 18.0,
       "y", 16.0, "wid", 80.0, "hi", 500.0, 0);
  fn9 (fn10 (v1), fn6 (), "x1", 0., "y1", 0., "x2", 80.0, "y2",
       500.0, "f", fn3 ("fff", 0x0D0DFA00), 0);
  fn13 (((S *) fn8 (v1, fn6 ()))->s);
  fn12 (fn8 (v1, fn11 ()), "ev", (T1) fn7, 0, fn8 (v1, fn6 ()));
  fn9 (fn8 (v1, fn6 ()), fn6 (), "wig",
       fn8 (v1, fn14 ()), "x", 111.0, "y", 14.0, "wid", 774.0, "hi",
       500.0, 0);
  v1 = fn9 (fn10 (v1), fn6 (), "x1", 0., "y1", 0., "x2", 774.0, "y2",
            500.0, "f", fn3 ("gc", 0x0D0DFA00), 0);
  fn1 (z, 0);
  i1 = fn9 (fn8 (v1, fn6 ()), fn6 (), "pixbuf", x, "x",
            800 - fn5 (x) / 2, "y", y - fn4 (x), 0);
  fn12 (fn8 (i1, fn11 ()), "ev", (T1) fn7, 0, "/ok/");
  fn12 (fn8 (i1, fn11 ()), "ev", (T1) fn7, 0, 0);
  i2 = fn9 (fn8 (v1, fn6 ()), fn6 (), "txt", "OK", "fnt", v2, "x",
            800, "y", y - fn4 (x) + 15, "ar", 0, "f", v3, 0);
}
