// { dg-do assemble  }

template < class T > class A
{
public:
  typedef typename T::myT anotherT; // { dg-error "" } undefined type

  anotherT t;

  A() { }
  A(anotherT _t) {
    t=_t;
  }

  anotherT getT() {
    return t;
  }
};

class B : public A< B > // { dg-error "" } forward declaration
{
public:
  typedef int myT;
};

int main() {
  B b;
}
