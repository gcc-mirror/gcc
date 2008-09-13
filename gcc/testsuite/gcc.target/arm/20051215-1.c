/* ARM's load-and-call patterns used to allow automodified addresses.
   This was wrong, because if the modified register were spilled,
   the call would need an output reload.  */
/* { dg-do run } */
/* { dg-options "-O2 -fno-omit-frame-pointer" } */
extern void abort (void);
typedef void (*callback) (void);

static void
foo (callback *first, callback *p)
{
  while (p > first)
    {
      (*--p) ();
#ifndef __thumb__
      asm ("" : "=r" (p) : "0" (p)
	   : "r4", "r5", "r6", "r7", "r8", "r9", "r10");
#endif
    }   
}

static void
dummy (void)
{
  static int count;
  if (count++ == 1)
    abort ();
}

int
main (void)
{
  callback list[1] = { dummy };
  foo (&list[0], &list[1]);
  return 0;
}
