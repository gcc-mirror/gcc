/* Simple check that sibling calls are performed from a
   void non-leaf-function taking one int argument calling a function which
   is about the same as itself.

   Copyright (C) 2002 Free Software Foundation Inc.
   Contributed by Hans-Peter Nilsson  <hp@bitrange.com>  */

/* { dg-do run { xfail arc-*-* avr-*-* cris-*-* crisv32-*-* h8300-*-* hppa*64*-*-* m32r-*-* m68hc1?-*-* mcore-*-* mn10300-*-* xstormy16-*-* v850*-*-* vax-*-* xtensa-*-* } } */
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

static ATTR void recurser_void1 (int);
static ATTR void recurser_void2 (int);
extern void track (int);

int main ()
{
  recurser_void1 (0);
  exit (0);
}

/* The functions should get the same stack-frame, and best way to make it
   reasonably sure is to make them have the same contents (regarding the
   n tests).  */

static void __attribute__((noinline)) ATTR
recurser_void1 (int n)
{
  if (n == 0 || n == 7 || n == 8)
    track (n);

  if (n == 10)
    return;

  recurser_void2 (n + 1);
}

static void __attribute__((noinline)) ATTR
recurser_void2 (int n)
{
  if (n == 0 || n == 7 || n == 8)
    track (n);

  if (n == 10)
    return;

  recurser_void1 (n + 1);
}

void *trackpoint;

void
track (int n)
{
  char stackpos[1];

  if (n == 0)
    trackpoint = stackpos;
  else if ((n != 7 && n != 8) || trackpoint != stackpos)
    abort ();
}
