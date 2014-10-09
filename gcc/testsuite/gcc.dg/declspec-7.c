/* Test declaration specifiers.  Test checks on storage class
   specifiers that can be made at parse time rather than for each
   declarator.  Note that __thread is tested in
   gcc.dg/tls/diag-*.c.  */
/* Origin: Joseph Myers <jsm@polyomino.org.uk> */
/* { dg-do compile } */
/* { dg-options "-fgnu89-inline" } */

/* Duplicate specifiers.  */

inline inline void f0 (void),
  f1 (void);

static static int a, /* { dg-error "duplicate 'static'" } */
  b;

extern extern int c, /* { dg-error "duplicate 'extern'" } */
  d;

typedef typedef int e, /* { dg-error "duplicate 'typedef'" } */
  f;

void
h (void)
{
  auto auto int p, /* { dg-error "duplicate 'auto'" } */
    q;

  register register int r, /* { dg-error "duplicate 'register'" } */
    s;
}

/* Multiple specifiers.  */

static extern int x, /* { dg-error "multiple storage classes in declaration specifiers" } */
  y;

extern typedef long z, /* { dg-error "multiple storage classes in declaration specifiers" } */
  w;
