// Build don't link:
// try throwing template function name

template <class T> void f(T);

template <class T> void f(T)
{
}

void g()
{
	throw &f; // ERROR - insufficient contextual information
}
