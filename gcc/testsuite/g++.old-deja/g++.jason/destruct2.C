// { dg-do assemble  }
// PRMS Id: 4342
// Bug: g++ does not massage things enough to allow calling ~X().

struct X 
{
    virtual ~X ();
};

struct Y : public X
{};

struct Z : public Y, public X	// { dg-warning "" }
{};

void foo ()
{
    Z* f = new Z;
    delete f;			// { dg-bogus "" } 
}
