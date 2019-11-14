/* { dg-do compile } */
/* { dg-require-alias "" } */
/* { dg-options "-O2 -fdump-ipa-icf-optimized -fmerge-all-constants -fdbg-cnt=merged_ipa_icf:1-2"  } */
/* { dg-prune-output "\\*\\*\\*dbgcnt:.*limit.*reached" } */

static int a;
static int b;
static const int c = 2;
static const int d = 2;
static char * e = "test";
static char * f = "test";
static int g[3]={1,2,3};
static int h[3]={1,2,3};
static const int *i=&c;
static const int *j=&c;
static const int *k=&d;
int t(int tt)
{
  switch (tt)
  {
    case 1: return a;
    case 2: return b;
    case 3: return c;
    case 4: return d;
    case 5: return e[1];
    case 6: return f[1];
    case 7: return g[1];
    case 8: return h[1];
    case 9: return i[0];
    case 10: return j[0];
    case 11: return k[0];
  }
}
/* { dg-final { scan-ipa-dump-times "Unified;" 2 "icf"  } } */
