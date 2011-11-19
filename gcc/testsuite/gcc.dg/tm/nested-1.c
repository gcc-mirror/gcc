/* { dg-do compile } */
/* { dg-options "-fgnu-tm" } */

extern int foo(int) __attribute__((transaction_safe));
void bar(void)
{
  __transaction_atomic {
    if (foo(1))
      __transaction_atomic {
	if (foo(2))
	  __transaction_atomic {
	    if (foo(3))
	      __transaction_atomic {
		if (foo(4))
		  foo(5);
		else
		  __transaction_cancel;
	      }
	    else
	      __transaction_cancel;
	  }
	else
	  __transaction_cancel;
      }
    else
      __transaction_cancel;
  }
}
