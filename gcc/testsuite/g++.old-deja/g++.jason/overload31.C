// { dg-do assemble  }
// PRMS Id: 6568
// Bug: g++ complains about the ambiguous conversion to bool even though
// we wouldn't end up using it anyway.

class AString
{
public:
    AString (unsigned len);
    operator char *() const;
    operator char const *() const;
};

void
foo(unsigned t, AString const & handle)
{
}

void
foo(AString const & handle, bool includeSpecials)
{
    unsigned t;
    foo(t, handle);
}
