// Build don't link:
// Origin: Jakub Jelinek <jakub@redhat.com>

class bar
{
public:
    bar();
    virtual ~bar();
    static void a();
};

class baz : public bar
{
};

class foo : virtual public baz
{
public:
    static void a();
    void b();
};

typedef void (bar::*T)();
T d;

void foo::a()
{
    typedef void(foo::*t)();
    t c = & foo::b;
    d = (T) c;			// WARNING - pointer to member cast
}
