// Check that template classes handle inherited virtual bases
// properly, initializing them before direct non-virtual bases.

int aflag;

struct A
{
  A() { aflag = 1; }
};

struct B : virtual public A 
{
  B() { }
};

struct C
{
  C() { if (!aflag) exit (1); }
};

template<class Parent>
struct D : public C, public Parent
{
  D() { }
};

int
main ()
{
  D<B> c;
}
