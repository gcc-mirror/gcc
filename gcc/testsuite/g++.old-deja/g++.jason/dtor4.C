// PRMS Id: 5420
// Bug: g++ gets mixed up calling destructors for references.
// Build don't link:

template<class X>
class Z {
public:
    char space[100];
    void kill()
	{ X& x = (X&) space[0];
	  x.~X(); }
};

class C { int i; };

void
f()
{
    Z<int> r;
    r.kill();
    Z<C> s;
    s.kill();
}
