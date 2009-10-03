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
f1 (bar<int> *p)
{
 p->m();
}

int
main ()
{
 return 0;
}
