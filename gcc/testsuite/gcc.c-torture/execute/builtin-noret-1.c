/* Test for builtin noreturn attributes.  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */

extern void abort (void);
extern void exit (int);
extern void _exit (int);
extern void _Exit (int);

extern void tabort (void);
extern void texit (void);
extern void t_exit (void);
extern void t_Exit (void);

extern void link_failure (void);

int
main (void)
{
  volatile int i = 0;
  /* The real test here is that the program links.  */
  if (i)
    tabort ();
  if (i)
    texit ();
  if (i)
    t_exit ();
  if (i)
    t_Exit ();
  exit (0);
}

void
tabort (void)
{
  abort ();
  link_failure ();
}

void
texit (void)
{
  exit (1);
  link_failure ();
}

void
t_exit (void)
{
  _exit (1);
  link_failure ();
}

/* Some non-Unix libcs might not have _exit.  This version should never
   get called.  */
static void
_exit (int i)
{
  abort ();
}

void
t_Exit (void)
{
  _Exit (1);
  link_failure ();
}

/* Some libcs might not have _Exit.  This version should never get called.  */
static void
_Exit (int i)
{
  abort ();
}

/* When optimizing, no calls to link_failure should remain.  In any case,
   link_failure should not be called.  */

#ifndef __OPTIMIZE__
void
link_failure (void)
{
  abort ();
}
#endif
