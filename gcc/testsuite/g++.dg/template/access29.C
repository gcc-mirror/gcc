// PR c++/22149

template < void (*FOOBAR) () >
class foo {
public:
    foo () { (*FOOBAR) (); }
};

class bar {
public:
    bar () { foo < bar::foobar > tmp; }
private:
    static void foobar ()
    {
    }
};

int
main ()
{
  bar b;
}
