// Bug: type_list_equal aborts when it sees lang-specific tree nodes.
// Build don't link:

struct A { };
void f (A a = A());
void g (A a = A());
