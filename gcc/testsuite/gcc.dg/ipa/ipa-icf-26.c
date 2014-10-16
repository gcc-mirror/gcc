/* { dg-do compile } */
/* { dg-options "-O2 -fdump-ipa-icf"  } */

void destroy (void)
{
}

void remove (void)
{
}


struct callbacks
{
  void (*success) (void);
  void (*error) (void);
};

struct callbacks my_callbacks;

__attribute__ ((noinline))
void foo()
{
  my_callbacks.success = destroy;
}

__attribute__ ((noinline))
void bar()
{
  my_callbacks.success = remove;
}

int main()
{
  foo();
  bar();

  return 0;
}

/* { dg-final { scan-ipa-dump "Semantic equality hit:bar->foo" "icf"  } } */
/* { dg-final { scan-ipa-dump "Semantic equality hit:remove->destroy" "icf"  } } */
/* { dg-final { scan-ipa-dump "Equal symbols: 2" "icf"  } } */
/* { dg-final { cleanup-ipa-dump "icf" } } */
