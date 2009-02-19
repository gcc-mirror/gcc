/* Undefined behavior from a call to a function cast to a different
   type does not appear until after the function designator and
   arguments have been evaluated.  PR 38483.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */

extern void exit (int);
extern void abort (void);

int
foo (void)
{
  exit (0);
  return 0;
}

void
bar (void)
{
}

int
main (void)
{
  ((long (*)(int))bar) (foo ());
  abort ();
}
