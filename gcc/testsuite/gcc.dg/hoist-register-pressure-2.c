/* { dg-options "-Os -fdump-rtl-hoist" }  */
/* The rtl hoist pass requires that the expression to be hoisted can
   be assigned without clobbering cc.  For a PLUS rtx on S/390 this
   requires a load address instruction which is fine on 64 bit but
   cannot be used on 31 bit since it does a 31 bit add only.  */
/* { dg-final { scan-rtl-dump "PRE/HOIST: end of bb .* copying expression" "hoist" { target { ! s390-*-* } } } } */
/* { dg-final { cleanup-rtl-dump "hoist" } } */

#define BUF 100
long a[BUF];

void com (long);
void bar (long);

long foo (long x, long y, long z)
{
  /* "x+y" won't be hoisted if "-fira-hoist-pressure" is disabled,
     because its rtx_cost is too small.  */
  if (z)
    {
      a[1] = a[0];
      a[2] = a[1];
      a[3] = a[3];
      a[4] = a[5];
      a[5] = a[7];
      a[6] = a[11];
      a[7] = a[13];
      a[8] = a[17];
      com (x+y);
    }
  else
    {
      bar (x+y);
    }

  return 0;
}
