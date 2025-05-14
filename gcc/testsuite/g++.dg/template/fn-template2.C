// PR c++/118516
// { dg-do compile }

int foo();
int main()
{
    for (int id = 0; i <		// { dg-error "not declared in this scope" }
			 foo(); ++id); // { dg-bogus "call to non-.constexpr." }
}
