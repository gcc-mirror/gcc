// Contributed by Dodji Seketeli <dodji@redhat.com>
// Origin PR c++/39987
// { dg-do compile }

class foo
{
 template<typename U>
 static bool func(const U& x)
 { return true; }
public:
 template<typename U>
 unsigned int Find(const U& x, bool (*pFunc) (const U&) = func) const
 { return 0; }
};

class bar {
 bool Initialize();
protected:
 foo b;
};

bool bar::Initialize()
{
        b.Find(b);
	return false;
}

