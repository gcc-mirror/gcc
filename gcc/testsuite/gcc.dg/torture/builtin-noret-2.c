/* Test for builtin noreturn attributes when the visible declarations
   are function-local.  Modified from builtin-noret-1.c by Zack Weinberg
   <zack@codesourcery.com>.  */
/* { dg-do link } */

extern void tabort (void);
extern void texit (void);
extern void t_exit (void);
extern void t_Exit (void);

extern void link_failure (void);

int
main (void)
{
  volatile int i = 0;
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
  extern void abort (void);
  abort ();
  link_failure ();
}

void
texit (void)
{
  extern void exit (int);
  exit (1);
  link_failure ();
}

void
t_exit (void)
{
  extern void _exit (int);
  _exit (1);
  link_failure ();
}

/* Some non-Unix libcs might not have _exit.  */
void
_exit (int i)
{
  abort ();
}

void
t_Exit (void)
{
  extern void _Exit (int);
  _Exit (1);
  link_failure ();
}

/* Some libcs might not have _Exit.  */
void
_Exit (int i)
{
  abort ();
}
