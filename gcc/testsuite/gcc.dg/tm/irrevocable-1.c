/* { dg-do compile } */
/* { dg-options "-fgnu-tm -O" } */

int global;
int george;

extern void crap() __attribute__((transaction_unsafe));

void
foo()
{
	__transaction_relaxed {
		global++;
		crap();
		george++;
	}
}
