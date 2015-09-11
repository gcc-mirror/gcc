/* { dg-do compile } */
/* { dg-options "-O2 -fdump-ipa-icf"  } */

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
/* { dg-final { scan-ipa-dump "Semantic equality hit:f2->f1" "icf"  } } */
/* { dg-final { scan-ipa-dump "Semantic equality hit:d->c" "icf"  } } */
/* { dg-final { scan-ipa-dump "Semantic equality hit:b->a" "icf"  } } */
