// { dg-do link }

extern void link_error();

int main()
{
	if (! __builtin_constant_p (&"Hello"[0]))
		link_error();
	return 0;
}
