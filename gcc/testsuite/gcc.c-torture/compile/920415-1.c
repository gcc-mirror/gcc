/* { dg-require-effective-target indirect_jumps } */
/* { dg-require-effective-target label_values } */

extern void abort (void);
extern void exit (int);

f ()
{
  __label__ l;
  void *x()
    {
      return &&l;
    }
  goto *x ();
  abort ();
  return;
 l:
  exit (0);
}
