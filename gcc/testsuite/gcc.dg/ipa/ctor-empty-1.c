/* { dg-do compile } */
/* { dg-options "-O3 -c -fdump-ipa-cgraph"  } */
static __attribute__((constructor))
void empty_constructor()
{
}
/* { dg-final { scan-ipa-dump "Reclaiming functions: empty_constructor"  "cgraph"  } } */
/* { dg-final { cleanup-ipa-dump "cgraph" } } */
