// { dg-do assemble  }
// { dg-options "-w" }
// Origin: r.spatschek@fz-juelich.de

class A
{
private:
  template <class T> void g(T t)  {} // { dg-error "" } private
  int i;
};

template <>
void A::g<int>(int t) { i = 1; }

int main()
{
  A a;
 
  a.g<int>(0); // { dg-error "" } private
}
