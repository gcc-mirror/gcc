// Test that the parser doesn't go into an infinite loop from ignoring the
// PRE_PARSED_FUNCTION_DECL token.

class C { static void* operator new(size_t); }; // { dg-error "" }
void* C::operator new(size_t) { return 0; } // { dg-error "" }
class D { D(int i): integer(i){}}; // { dg-error "" }
