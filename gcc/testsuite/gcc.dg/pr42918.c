/* PR debug/42918 */
/* { dg-do compile } */
/* { dg-options "-O2 -fcompare-debug -ftracer" } */

extern int fi (void);
extern void fv (void);

int
f (int i, int j)
{
  if (!j)
    {
      fv ();
      goto lab;
    }
  i = fi ();
  if (i == j)
    fv ();
lab:
  return i;
}
