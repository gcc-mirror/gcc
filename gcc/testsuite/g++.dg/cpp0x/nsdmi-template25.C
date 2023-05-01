// PR c++/106890
// { dg-do compile { target c++11 } }

struct A
{
  int p;
};

template<typename T>
struct B : virtual public A
{
  B() { }
  B(int) { }

  int k = this->p;
};

template struct B<int>;
