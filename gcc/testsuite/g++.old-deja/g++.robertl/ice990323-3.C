// { dg-do assemble  }
// try throwing overloaded function

void f(int)
{
}

void f(long)
{
}

void g()
{
	throw &f; // { dg-error "" } insufficient contextual information
}
