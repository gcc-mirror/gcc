/* { dg-do compile } */
/* { dg-options "-O2 -fdump-ipa-icf-optimized -fno-inline"  } */

void destroy (void)
{
  __asm__ __volatile__ ("" : : : "memory");
}

void remove (void)
{
  __asm__ __volatile__ ("" : : : "memory");
}

void remove2 (void)
{
  __asm__ __volatile__ ("" : : : );
}

int main()
{
  destroy ();
  remove ();
  remove2 ();

  return 0;
}

/* { dg-final { scan-ipa-dump "Semantic equality hit:destroy/\[0-9+\]+->remove/\[0-9+\]+" "icf"  } } */
/* { dg-final { scan-ipa-dump "Equal symbols: 1" "icf"  } } */
