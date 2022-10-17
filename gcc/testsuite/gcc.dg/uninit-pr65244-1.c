/* { dg-do compile } */
/* { dg-options "-O -Wuninitialized" } */

extern void __attribute__((noreturn)) abort (void);

int foo (int flag, int val)
{
  int tem;
  if (flag)
    {
      if (val == 0)
        abort ();
      tem = val;
    }
  /* large - prevent jump threading */
  __asm__ volatile ("\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n");
  if (flag)
    return tem; /* { dg-bogus "uninitialized" } */
  return 0;
}
