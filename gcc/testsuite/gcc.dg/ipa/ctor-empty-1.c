/* { dg-do compile } */
/* { dg-options "-O3 -c -fdump-ipa-free-inline-summary"  } */
static __attribute__((constructor))
void empty_constructor()
{
}
/* { dg-final { scan-ipa-dump "Reclaiming functions: empty_constructor"  "free-inline-summary"  } } */
