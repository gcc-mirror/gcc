// Build don't link: 
// GROUPS passed old-abort
#include <iostream.h>

	void
	fubar(ostream* out, const char* string)
	{
  	  (*out) << string << endl;
  	  return;
	}

	int
	main()
	{
  	  // Declare a ref and a pointer to the same ostream.
  	  //
  	  ostream* out = &cerr;
  	  ostream& die = cerr;

  	  // Valid call to fubar.
  	  //
  	  fubar(out, "First line.");
  
  	  // Invalid call to fubar. (1st arg is an ostream&. fubar expects
  	  // ostream*.)This should be a syntax error, but g++ does not catch it.
  	  // Call to this function results in a bus error in fubar when the 1st
  	  // arg is dereferenced.
  	  //
  	  fubar(die, "Second line.");// ERROR -  cannot convert .die.*
  
  	  return 1;
	}
