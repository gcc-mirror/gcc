/* Test that stack alignment is preserved with pending_stack_adjust
   with stdcall functions.  */

/* { dg-do run { target i?86-*-* } } */
/* { dg-options -mpreferred-stack-boundary=4 } */

void __attribute__((stdcall)) foo(int a, int b, int c);

extern void abort (void);
extern void exit (int);

int
main ()
{
  foo(1, 2, 3);
  foo(1, 2, 3);
  exit (0);
}

void __attribute__((stdcall))
foo(int a, int b, int c)
{
  static int last_align = -1;
  int dummy, align = (int)&dummy & 15;
  if (last_align < 0)
    last_align = align;
  else if (align != last_align)
    abort ();
}
