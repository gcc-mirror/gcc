/* { dg-do compile } */
/* { dg-options "-O2 -fdump-ipa-sra"  } */

namespace {

class C
{

  int mi;

public:
  C (int i)
    : mi(i)
  {}

  void foo (int c);
};

volatile int vi;


void __attribute__((noinline))
C::foo (int cond)
{
  int i;
  if (cond)
    i = mi;
  else
    i = 0;
  vi = i;
}

static C c_instance(1);
}

void __attribute__((noinline))
bar (C *p, int cond)
{
  p->foo (cond);
}


class C *gp;

void something(void);

void
baz (int cond)
{
  C c(vi);
  gp = &c;
  something ();
  bar (gp, cond);
}

void
hoo(void)
{
  gp = &c_instance;
}

/* { dg-final { scan-ipa-dump "Will split parameter" "sra" } } */
