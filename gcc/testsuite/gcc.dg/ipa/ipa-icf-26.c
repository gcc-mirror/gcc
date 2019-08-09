/* { dg-do compile } */
/* { dg-options "-O2 -fdump-ipa-icf-optimized"  } */

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

/* { dg-final { scan-ipa-dump "Semantic equality hit:destroy->remove" "icf"  } } */
/* { dg-final { scan-ipa-dump "Equal symbols: 1" "icf"  } } */
