// Build don't link:

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
	pmf = & pf->f; // ERROR - not a valid pmf expression
}
