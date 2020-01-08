/* { dg-do compile } */
/* { dg-options "-O2 -fdump-ipa-icf-optimized-all-all"  } */

void f1()
{
}

void f2()
{
}

static void (*a)(void)=&f1;
static void (*b)(void)=&f1;
static void (*c)(void)=&f2;
static void (*d)(void)=&f2;

int main()
{
  a();
  b();
  c();
  d();

  return 0;
}

/* { dg-final { scan-ipa-dump "Equal symbols: 3" "icf"  } } */
/* { dg-final { scan-ipa-dump "Semantic equality hit:f1/\[0-9+\]+->f2/\[0-9+\]+" "icf"  } } */
/* { dg-final { scan-ipa-dump "Semantic equality hit:c/\[0-9+\]+->d/\[0-9+\]+" "icf"  } } */
/* { dg-final { scan-ipa-dump "Semantic equality hit:a/\[0-9+\]+->b/\[0-9+\]+" "icf"  } } */
