// { dg-do assemble  }
// PRMS Id: 5190
// Bug: g++ fails to build up a const reference to `this'.

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
