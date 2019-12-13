/* Test for uses of incomplete enum variables: should be allowed just
   when incomplete structs are allowed.  PR 32295.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "" } */

extern enum e ve;
extern struct s vs;
void *p;

int
f0 (int i)
{
  ve; /* { dg-error "incomplete" } */
  vs; /* { dg-error "incomplete" } */
  (void) ve; /* { dg-error "incomplete" } */
  (void) vs; /* { dg-error "incomplete" } */
  (void) (i ? ve : ve); /* { dg-error "incomplete" } */
  (void) (i ? vs : vs); /* { dg-error "incomplete" } */
  (void) (ve = ve); /* { dg-error "incomplete" } */
  (void) (vs = vs); /* { dg-error "incomplete" } */
  (void) ve, /* { dg-error "incomplete" } */
    (void) ve; /* { dg-error "incomplete" } */
  (void) vs, /* { dg-error "incomplete" } */
    (void) vs; /* { dg-error "incomplete" } */
  p = &ve;
  p = &vs;
  (void) sizeof (ve); /* { dg-error "incomplete" } */
  (void) sizeof (vs); /* { dg-error "incomplete" } */
  typeof (ve) *pe;
  typeof (vs) *ps;
  /* ??? alignof should probably not be accepted here.  */
  (void) __alignof (ve);
  (void) __alignof (vs);
  (void) (ve + i); /* { dg-error "incomplete" } */
  (void) (i * ve); /* { dg-error "incomplete" } */
  (void) (i / ve); /* { dg-error "incomplete" } */
  (void) (ve - i); /* { dg-error "incomplete" } */
  (void) (ve << i); /* { dg-error "incomplete" } */
  (void) (i >> ve); /* { dg-error "incomplete" } */
  (void) (ve < i); /* { dg-error "incomplete" } */
  (void) (ve <= i); /* { dg-error "incomplete" } */
  (void) (i > ve); /* { dg-error "incomplete" } */
  (void) (i >= ve); /* { dg-error "incomplete" } */
  (void) (ve == i); /* { dg-error "incomplete" } */
  (void) (i != ve); /* { dg-error "incomplete" } */
  (void) (ve & i); /* { dg-error "incomplete" } */
  (void) (ve ^ i); /* { dg-error "incomplete" } */
  (void) (i | ve); /* { dg-error "incomplete" } */
  (void) (i && ve); /* { dg-error "incomplete" } */
  (void) (ve || i); /* { dg-error "incomplete" } */
  (void) -ve; /* { dg-error "incomplete" } */
  (void) +ve; /* { dg-error "incomplete" } */
  (void) ~ve; /* { dg-error "incomplete" } */
  (void) !ve; /* { dg-error "incomplete" } */
  (void) --ve; /* { dg-error "incomplete" } */
  (void) ++ve; /* { dg-error "incomplete" } */
  (void) ve--; /* { dg-error "incomplete" } */
  (void) ve++; /* { dg-error "incomplete" } */
  i = ve; /* { dg-error "incomplete" } */
  i *= ve; /* { dg-error "incomplete" } */
  i /= ve; /* { dg-error "incomplete" } */
  i %= ve; /* { dg-error "incomplete" } */
  i += ve; /* { dg-error "incomplete" } */
  i -= ve; /* { dg-error "incomplete" } */
  i <<= ve; /* { dg-error "incomplete" } */
  i >>= ve; /* { dg-error "incomplete" } */
  i &= ve; /* { dg-error "incomplete" } */
  i ^= ve; /* { dg-error "incomplete" } */
  i |= ve; /* { dg-error "incomplete" } */
  (void) (ve ? 1 : 1); /* { dg-error "incomplete" } */
  (void) (int) ve; /* { dg-error "incomplete" } */
  f0 (ve); /* { dg-error "incomplete" } */
  if (ve) /* { dg-error "incomplete" } */
    ;
  do
    ;
  while (ve); /* { dg-error "incomplete" } */
  while (ve) /* { dg-error "incomplete" } */
    ;
  _Bool b = ve; /* { dg-error "incomplete" } */
  float f = ve; /* { dg-error "incomplete" } */
  switch (ve) /* { dg-error "incomplete" } */
    ;
  for (; ve;) /* { dg-error "incomplete" } */
    ;
  return ve; /* { dg-error "incomplete" } */
}
