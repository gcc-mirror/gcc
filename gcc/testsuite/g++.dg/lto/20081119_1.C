class foo {
 public:
 foo () {}
 virtual ~foo() {}
 virtual void m() {}
};

template<typename t>
class bar : public foo {
 public:
 bar () {}
};

void
f2 (bar<int> *p)
{
 p->m();
}
