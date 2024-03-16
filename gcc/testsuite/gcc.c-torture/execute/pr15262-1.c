/* PR 15262.
   The alias analyzer only considers relations between pointers and
   symbols.  If two pointers P and Q point to the same symbol S, then
   their respective memory tags will either be the same or they will
   have S in their alias set.
   
   However, if there are no common symbols between P and Q, TBAA will
   currently miss their alias relationship altogether.  */

void abort (void);

struct A
{
  int t;
  int i;
};

int foo () { return 3; }

int
main (void)
{
  struct A loc, *locp;
  float f, g, *p;
  int T355, *T356;

  /* Avoid the partial hack in TBAA that would consider memory tags if
     the program had no addressable symbols.  */
  f = 3;
  g = 2;
  p = foo () ? &g : &f;
  if (*p > 0.0)
    g = 1;

  /* Store into *locp and cache its current value.  */
  locp = __builtin_malloc (sizeof (*locp));
  locp->i = 10;
  T355 = locp->i;

  /* Take the address of one of locp's fields and write to it.  */
  T356 = &locp->i;
  *T356 = 1;

  /* Read the recently stored value.  If TBAA fails, this will appear
     as a redundant load that will be replaced with '10'.  */
  T355 = locp->i;
  if (T355 != 1)
    abort ();

  return 0;
}
