// Build don't link:

template <char* c> struct B { B() { } };
B<0> bnull; // ERROR - could not convert template argument
