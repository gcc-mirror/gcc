/* Test miscellaneous uses of UCNs in identifiers compile and run OK,
   with debug info enabled.  */
/* { dg-do run } */
/* { dg-options "-std=c99 -g" } */

extern void abort (void);
extern void exit (int);

int
main (void)
{
  struct \u00C0 { int \u00C1; } x;
  struct \u00C0 *y = &x;
  y->\u00C1 = 1;
  if (x.\U000000C1 != 1)
    abort ();
  goto \u00ff;
 \u00ff: ;
  enum e { \u00C2 = 4 };
  if (\u00C2 != 4)
    abort ();
  exit (0);
}
