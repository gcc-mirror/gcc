// { dg-do assemble  }

template < class T > class A
{
public:
  typedef typename T::myT anotherT; // { dg-error "" } undefined type

  anotherT t; // { dg-error "" } undefined type 

  A() { }
  A(anotherT _t) { // { dg-error "" } undefined type
    t=_t;
  }

  anotherT getT() { // { dg-error "" } undefined type
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
