// { dg-do assemble  }
// { dg-skip-if "requires hosted libstdc++ for iostream" { ! hostedlib } }
// GROUPS passed old-abort
#include <iostream>

	void
	fubar(std::ostream* out, const char* s)
	{
  	  (*out) << s << std::endl;
  	  return;
	}

	int
	main()
	{
  	  // Declare a ref and a pointer to the same ostream.
  	  //
  	  std::ostream* out = &std::cerr;
  	  std::ostream& die = std::cerr;

  	  // Valid call to fubar.
  	  //
  	  fubar(out, "First line.");
  
  	  // Invalid call to fubar. (1st arg is an ostream&. fubar expects
  	  // ostream*.)This should be a syntax error, but g++ does not catch it.
  	  // Call to this function results in a bus error in fubar when the 1st
  	  // arg is dereferenced.
  	  //
  	  fubar(die, "Second line.");// { dg-error "" }  cannot convert .die.*
  
  	  return 1;
	}
