/* Test for builtin noreturn attributes.  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-options "-multiply_defined suppress" { target powerpc-*-darwin* } } */
/* { dg-do link } */

extern void abort (void);
extern void exit (int);
extern void _exit (int);
extern void _Exit (int);

extern void tabort (void);
extern void texit (void);
extern void t_exit (void);
extern void t_Exit (void);

extern void link_failure (void);

/* Some libcs have _exit and/or _Exit, and won't allow it to be re-defined,
   so make it weak.  */
#pragma weak _exit
#pragma weak _Exit

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

/* Some non-Unix libcs might not have _exit.  */
void
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

/* Some libcs might not have _Exit.  */
void
_Exit (int i)
{
  abort ();
}
