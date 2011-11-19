/* { dg-do compile } */
/* { dg-options "-fgnu-tm" } */

void mco(void) __attribute__((transaction_may_cancel_outer));

void
f(void)
{
  mco();			/* { dg-error "" } */
  __transaction_atomic {
    mco();			/* { dg-error "" } */
  }
  __transaction_relaxed {
    mco();			/* { dg-error "" } */
  }
  __transaction_atomic [[outer]] {
    mco();
  }
  __transaction_atomic [[outer]] {
    __transaction_atomic {
      __transaction_atomic {
	__transaction_atomic {
	  mco();
	}
      }
    }
  }
}

void __attribute__((transaction_may_cancel_outer))
g(void)
{
  mco();
  __transaction_atomic {
    __transaction_atomic {
      __transaction_atomic {
	__transaction_atomic {
	  mco();
	}
      }
    }
  }
}
