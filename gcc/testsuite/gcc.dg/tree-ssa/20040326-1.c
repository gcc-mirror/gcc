/* { dg-options "-O2 -fno-inline-functions" } */
/* { dg-do run } */
/* When there are no call-clobbered variables, we should still create
   a .GLOBAL_VAR to model the side effects of functions.  Without it,
   we were moving the call to Faref() inside the second call to
   Faset().  */
extern void abort (void);
extern void exit (int);

main ()
{
  int table, c, elt;
  int tem = Faref (table, elt);
  Faset (table, elt, c);
  Faset (table, c, tem);/* tem cannot be replaced with Faref (table, elt) */
  exit (0);
}

int j = 0;

int __attribute__ ((noinline)) Faref (table, elt)
{
  j = 1;
  return 0;
}

int __attribute__ ((noinline)) Faset (table, elt, c)
{
  if (j != 1)
    abort ();
  return 0;
}
