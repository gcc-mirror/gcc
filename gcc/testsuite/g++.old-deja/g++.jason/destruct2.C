// PRMS Id: 4342
// Bug: g++ does not massage things enough to allow calling ~X().
// Build don't link:

struct X 
{
    virtual ~X ();
};

struct Y : public X
{};

struct Z : public Y, public X
{};				// WARNING - 

void foo ()
{
    Z* f = new Z;
    delete f;			// gets bogus error - 
}
