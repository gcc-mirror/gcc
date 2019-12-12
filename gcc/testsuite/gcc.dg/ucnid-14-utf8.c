/* Test miscellaneous uses of UTF-8 in identifiers compile and run OK,
   with debug info enabled.  */
/* { dg-do run } */
/* { dg-options "-std=c99 -g" } */

extern void abort (void);
extern void exit (int);

int
main (void)
{
  struct À { int Á; } x;
  struct À *y = &x;
  y->Á = 1;
  if (x.Á != 1)
    abort ();
  goto ÿ;
 ÿ: ;
  enum e { Â = 4 };
  if (Â != 4)
    abort ();
  exit (0);
}
