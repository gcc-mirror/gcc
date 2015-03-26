// PR c++/65525

struct A
{
    int x;
    char y; // Actually, short and bool (types smaller than int?) trigger this ICE too
    // Also: the problem doesn't occur if you put the smaller type first, e.g. "char x; int y;"

    A(int x) {} // custom ctor needed for ICE
};

int main()
{
    A a(0), x(1), y(2);

    x = a; // OK
    y = a; // OK
    x = y = a; // ICE: sorry, unimplemented: unexpected AST of kind mem_ref
    // internal compiler error: in potential_constant_expression_1, at cp/constexpr.c:4432

    return 0;
}
