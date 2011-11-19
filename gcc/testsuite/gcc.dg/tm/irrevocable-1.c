/* { dg-do compile } */
/* { dg-options "-fgnu-tm -O" } */

int global;
int george;

extern crap() __attribute__((transaction_unsafe));

foo()
{
	__transaction_relaxed {
		global++;
		crap();
		george++;
	}
}
