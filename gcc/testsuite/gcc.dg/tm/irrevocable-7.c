/* { dg-do compile } */
/* { dg-options "-fgnu-tm -fdump-ipa-tmipa" } */

extern void bark(void);

__attribute__((transaction_callable))
int foo()
{
      bark();
}

/* { dg-final { scan-ipa-dump-times "changeTransactionMode \\(0\\)" 1 "tmipa" } } */
/* { dg-final { cleanup-ipa-dump "tmipa" } } */
