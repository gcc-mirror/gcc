/* An aggregate copy into the return value is eliminated by inlining only
   when its source is a local variable or a parameter whose address is not
   taken.  */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-ipa-fnsummary-details -fno-early-inlining -fno-partial-inlining -fno-ipa-cp" } */

struct V { long d[8]; };
struct H { struct V *p; };
struct V glob;
void sink (struct V *);

struct V
from_local (const struct V *a, const struct V *b)
{
  struct V loc = *a;
  for (int i = 0; i < 8; i++)
    loc.d[i] *= b->d[i];
  return loc;
}

struct V
from_parm (struct V parm, const struct V *b)
{
  for (int i = 0; i < 8; i++)
    parm.d[i] *= b->d[i];
  return parm;
}

struct V
from_unmodified_parm (struct V uparm)
{
  return uparm;
}

struct V
from_escaped_local (const struct V *a)
{
  struct V esc = *a;
  sink (&esc);
  return esc;
}

struct V
from_global (const struct V *b)
{
  for (int i = 0; i < 8; i++)
    glob.d[i] *= b->d[i];
  return glob;
}

struct V
from_static (const struct V *b)
{
  static struct V stat;
  for (int i = 0; i < 8; i++)
    stat.d[i] += b->d[i];
  return stat;
}

struct V
from_pointer (const struct H *h, const struct V *b)
{
  struct V *p = h->p;
  for (int i = 0; i < 8; i++)
    p->d[i] *= b->d[i];
  return *p;
}

/* { dg-final { scan-ipa-dump {<retval> = loc;\n[^\n]*\n\t\tWill be eliminated by inlining} "fnsummary" } } */
/* { dg-final { scan-ipa-dump {<retval> = parm;\n[^\n]*\n\t\tWill be eliminated by inlining} "fnsummary" } } */
/* { dg-final { scan-ipa-dump {<retval> = uparm;\n[^\n]*\n\t\t50% will be eliminated by inlining} "fnsummary" } } */
/* { dg-final { scan-ipa-dump-not {<retval> = esc;\n[^\n]*\n\t\tWill be eliminated by inlining} "fnsummary" } } */
/* { dg-final { scan-ipa-dump-not {<retval> = glob;\n[^\n]*\n\t\tWill be eliminated by inlining} "fnsummary" } } */
/* { dg-final { scan-ipa-dump-not {<retval> = stat;\n[^\n]*\n\t\tWill be eliminated by inlining} "fnsummary" } } */
/* { dg-final { scan-ipa-dump-not {<retval> = \*p_[0-9]+;\n[^\n]*\n\t\tWill be eliminated by inlining} "fnsummary" } } */
