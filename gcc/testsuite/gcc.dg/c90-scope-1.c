/* Test for new block scopes in C99.  Inspired by C99 Rationale (N897).  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do run } */
/* { dg-options "-std=iso9899:1990 -pedantic-errors" } */

struct foo {
  char a;
};

extern void abort (void);
extern void exit (int);

int
sfoo (void)
{
  if (sizeof (struct foo { int a; double b; char *c; void *d; }))
    (void) 0;
  return sizeof (struct foo);
}

int
main (void)
{
  int t, u;
  t = sfoo ();
  u = sizeof (struct foo);
  /* With C90 scoping rules the new declaration of struct foo is in scope
     above; with C99 it is local to the if.
  */
  if (t == u)
    abort (); /* C99 rules apply.  */
  else
    exit (0); /* C90 rules apply.  */
}
