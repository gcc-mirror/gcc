// { dg-do assemble  }

// submitted by David C Binderman <dcb@pncl.co.uk>

typedef const int ci;
typedef ci aci[ 10];
aci var = { 2, 3, 5, 7, 11, 13 };

void
f()
{
	int * ip = var;	// { dg-error "" } requires const_cast
}
