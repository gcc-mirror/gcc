/* Simple check that sibling calls are performed from a
   void non-leaf-function taking no arguments calling a function which
   is about the same as itself.

   Copyright (C) 2002 Free Software Foundation Inc.
   Contributed by Hans-Peter Nilsson  <hp@bitrange.com>  */

/* { dg-do run { xfail { { cris-*-* crisv32-*-* h8300-*-* hppa*64*-*-* m32r-*-* mcore-*-* mn10300-*-* msp430*-*-* nds32*-*-* xstormy16-*-* v850*-*-* vax-*-* xtensa*-*-* } || { arm*-*-* && { ! arm32 } } } } } */
/* -mlongcall disables sibcall patterns.  */
/* { dg-skip-if "" { powerpc*-*-* } { "-mlongcall" } { "" } } */
/* { dg-options "-O2 -foptimize-sibling-calls" } */

/* The option -foptimize-sibling-calls is the default, but serves as
   marker.  This test is xfailed on targets without sibcall patterns
   (except targets where the test does not work due to the return address
   not saved on the regular stack).  */

extern void abort (void);
extern void exit (int);

/* Sibcalls are not supported in MIPS16 mode, which has direct calls but
   not direct jumps.  */
#ifdef __mips
#define ATTR __attribute__((nomips16))
#else
#define ATTR
#endif

static ATTR void recurser_void1 (void);
static ATTR void recurser_void2 (void);
extern void track (void);

int n = 0;
int main ()
{
  recurser_void1 ();
  exit (0);
}

/* The functions should get the same stack-frame, and best way to make it
   reasonably sure is to make them have the same contents (regarding the
   n tests).  */

static void __attribute__((noinline)) ATTR
recurser_void1 (void)
{
  if (n == 0 || n == 7 || n == 8)
    track ();

  if (n == 10)
    return;
  n++;
  recurser_void2 ();
}

static void __attribute__((noinline)) ATTR
recurser_void2 (void)
{
  if (n == 0 || n == 7 || n == 8)
    track ();

  if (n == 10)
    return;
  n++;
  recurser_void1 ();
}

void *trackpoint;

void __attribute__ ((noinline))
track ()
{
  char stackpos[1];

  if (n == 0)
    trackpoint = stackpos;
  else if ((n != 7 && n != 8) || trackpoint != stackpos)
    abort ();
}
