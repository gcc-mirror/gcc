// Make sure we can initialize a reference to a templated type, that
// requires a conversion from a derived type to a base type.

// Build don't link:
// prms-id: 3524

struct cc2Vector
{};

template <class T>
struct ccPair
{
    ccPair (const cc2Vector&);
};

struct ccLine  : cc2Vector
{
    double distToPoint  (const ccPair <float> &);
};

void foo ()
{
    ccLine l2;
    l2.distToPoint (l2);
}
