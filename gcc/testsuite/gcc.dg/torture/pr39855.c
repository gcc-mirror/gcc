/* PR c/39855 */
/* { dg-do run { target { int32plus } } } */

extern void abort (void);

int i, j, k;

int
foo (void)
{
  return ++i;
}

int
main ()
{
  if (__CHAR_BIT__ != 8 || sizeof (int) != 4)
    return 0;
  j = foo () << 30 << 2;
  k = (unsigned) foo () >> 16 >> 16;
  if (i != 2 || j != 0 || k != 0)
    abort ();
  return 0;
}
