/* { dg-do compile } */
/* { dg-options "-mabi=lp64d -O2 -fdump-rtl-combine" } */
/* { dg-final { scan-rtl-dump "{bstrpick_alsl_paired}" "combine" } } */
/* { dg-final { scan-assembler-not "alsl.d\t\\\$r\[0-9\]+,\\\$r\[0-9\]+,\\\$r0" } } */

struct SA
{
  const char *a;
  unsigned int b : 16;
  unsigned int c : 16;
};

extern struct SA SAs[];

void
test ()
{
  unsigned int i;
  for (i = 0; i < 100; i++)
    SAs[i].c = i;
}
