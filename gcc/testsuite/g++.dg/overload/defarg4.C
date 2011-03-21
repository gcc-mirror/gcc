// Contributed by Dodji Seketeli <dodji@redhat.com>
// Origin PR c++/39987
// { dg-do compile }

class foo
{
 template<typename U>
 static bool func(const U& x)
 {}
public:
 template<typename U>
 unsigned int Find(const U& x, bool (*pFunc) (const U&) = func) const
 {}
};

class bar {
 bool Initialize();
protected:
 foo b;
};

bool bar::Initialize()
{
        b.Find(b);
}

