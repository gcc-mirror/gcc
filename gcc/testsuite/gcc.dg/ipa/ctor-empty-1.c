/* { dg-do compile } */
/* { dg-options "-O3 -c -fdump-ipa-free-fnsummary1"  } */
static __attribute__((constructor))
void empty_constructor()
{
}
/* { dg-final { scan-ipa-dump "Reclaiming functions: empty_constructor"  "free-fnsummary1"  } } */
