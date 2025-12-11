/* { dg-do compile } */
/* { dg-options "-mabi=lp64d -O2" } */
/* { dg-final { scan-assembler-not "slli.w\t\\\$r\[0-9\]+,\\\$r\[0-9\]+,0" } } */

extern int items;
extern int gv_fetchmeth (int);
int
Perl_gv_fetchmeth (int level)
{
  int gv;
  while (items--)
    gv = gv_fetchmeth ((level >= 0) ? level + 1 : level - 1);

  return gv;
}

