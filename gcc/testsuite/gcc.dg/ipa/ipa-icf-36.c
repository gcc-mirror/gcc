/* { dg-do compile } */
/* { dg-options "-O2 -fdump-ipa-icf -fmerge-all-constants"  } */
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
/* { dg-final { scan-ipa-dump "Equal symbols: 6" "icf"  } } */
/* { dg-final { scan-ipa-dump "Semantic equality hit:a->b" "icf"  } } */
/* { dg-final { scan-ipa-dump "Semantic equality hit:c->d" "icf"  } } */
/* { dg-final { scan-ipa-dump "Semantic equality hit:e->f" "icf"  } } */
/* { dg-final { scan-ipa-dump "Semantic equality hit:g->h" "icf"  } } */
/* { dg-final { scan-ipa-dump "Semantic equality hit:i->k" "icf"  } } */
