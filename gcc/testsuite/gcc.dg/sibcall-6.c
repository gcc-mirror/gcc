/* A simple check to see whether indirect calls are
   being sibcall optimized on targets that do support
   this notion, i.e. have the according call patterns
   in place.

   Copyright (C) 2002 Free Software Foundation Inc.
   Contributed by Andreas Bauer <baueran@in.tum.de>  */

/* { dg-do run { target epiphany-*-* i?86-*-* x86_64-*-* s390*-*-* } } */
/* { dg-skip-if "" { { i?86-*-* x86_64-*-* } && { ia32 && { ! nonpic } } } } */
/* { dg-options "-O2 -foptimize-sibling-calls -fno-ipa-cp" } */

extern void abort (void);
extern void exit (int);

int foo (int);
int bar (int);

int (*ptr) (int);
int *f_addr;

int
main ()
{
  ptr = bar;
  foo (7);
  exit (0);
}

int __attribute__ ((noinline))
bar (b)
     int b;
{
  if (f_addr == (int*) __builtin_return_address (0))
    return b;
  else
    abort ();
}

int __attribute__ ((noinline))
foo (f)
     int f;
{
  f_addr = (int*) __builtin_return_address (0);
  return (*ptr)(f);
}
