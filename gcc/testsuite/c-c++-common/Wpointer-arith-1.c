/* { dg-do compile } */
/* { dg-options "-Wpedantic -Wno-pointer-arith" } */

void h(void)
{
  typedef void (*pft) ();
  typedef void (ft) ();

  void *pv = 0;
  pft pf = 0;

  pv++;
  pf++;

  --pv;
  --pf;

  pv += 1;
  pf += 1;

  pv = pv - 1;
  pf = pf - 1;

  sizeof (void);
  sizeof (ft);
}
