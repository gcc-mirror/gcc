// PRMS Id: 5204
// Bug: g++ bashes the type of add_sym with the type of add, so calling it
// with one parameter generates an error.
// Build don't link:

int add(int const &symbol,
	const unsigned char flags=(void*)0); // ERROR - invalid default arg

int add_sym(int const &symbol,
	    const unsigned char flags=0);

int main()
{
   int fname;
   add_sym(fname);      // Guarantee a symbol exists
}
