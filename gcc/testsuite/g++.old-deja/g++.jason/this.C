// PRMS Id: 5190
// Bug: g++ fails to build up a const reference to `this'.
// Build don't link:

class X
{
public:
    void member ();
};

void print (const X* const &);

void X::member ()
{
    print (this);
}
