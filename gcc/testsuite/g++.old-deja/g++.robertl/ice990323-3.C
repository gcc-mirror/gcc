// Build don't link:
// try throwing overloaded function

void f(int)
{
}

void f(long)
{
}

void g()
{
	throw &f; // ERROR - insufficient contextual information
}
