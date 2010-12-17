/* { dg-do compile } */
/* { dg-options "-O3 -c -fdump-ipa-whole-program"  } */
static __attribute__((constructor))
void empty_constructor()
{
}
/* { dg-final { scan-ipa-dump "Reclaiming functions: empty_constructor"  "whole-program"  } } */
/* { dg-final { cleanup-ipa-dump "whole-program" } } */
