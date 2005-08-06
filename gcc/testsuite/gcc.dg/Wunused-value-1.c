/* Test -Wunused-value.  Bug 23113.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "-Wunused-value" } */

int f (void);
void g (void);
int *p;

void
h (void)
{
  1 + f (); /* { dg-warning "value computed is not used" } */
  f () + f (); /* { dg-warning "value computed is not used" } */
  f () + f (), f (); /* { dg-warning "value computed is not used" } */
  (char) f (); /* { dg-warning "value computed is not used" } */
  g ();
  f ();
  (void) f ();
  *p++; /* { dg-warning "value computed is not used" } */
  ++*p;
  (*p ? f() : 0);
  ({ f(); });
  /* Statement expressions may be used in macro expansions which like
     functions return values which may or may not be of use, so don't
     warn for them but do warn inside them.  */
  ({ f() + 1; });
  ({ f(); 0; });
  ({ f() + 1; 0; }); /* { dg-warning "value computed is not used" } */
  1 + ({ f(); }); /* { dg-warning "value computed is not used" } */
}
