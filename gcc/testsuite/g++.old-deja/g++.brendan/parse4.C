// Build don't link: 

// this is marked as an expected error because it evidences an
// ambiguity in the grammar between expressions and declarations.
// when the parser's been cleaned up or rewritten, the error
// marker can go away, since it'll no longer occur.

class B
{
public:
    B( int t ) {}
    void f() {}
};

int g() { return 0; } // gets bogus error - referenced below

int main()
{
    int try1;
    B( try1 ).f();   // no syntax error
    B b( g() );      // no syntax error
    B( ::g() ).f();  // gets bogus error - treated as decl XFAIL *-*-*
    B( g() ).f();    // gets bogus error - treated as decl XFAIL *-*-*
}
