// { dg-do assemble  }

// submitted by David C Binderman <dcb@pncl.co.uk>

struct S
{
	void f();
};

void (S ::* pmf) ();

S * pf;

void
f()
{
	pmf = & pf->f; // { dg-error "" } not a valid pmf expression
}
