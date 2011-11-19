/* { dg-do compile } */
/* { dg-options "-fgnu-tm -fdump-ipa-tmipa" } */

/* Test that irrevocability gets set for the obvious case.  */

int global;
int george;

extern crap() __attribute__((transaction_unsafe));

foo(){
    __transaction_relaxed {
	global++;
	crap();
	george++;
    }
}

/* { dg-final { scan-ipa-dump-times "GTMA_MAY_ENTER_IRREVOCABLE" 1 "tmipa" } } */
/* { dg-final { cleanup-ipa-dump "tmipa" } } */
