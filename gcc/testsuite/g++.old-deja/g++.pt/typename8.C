// Build don't link:

template < class T > class A
{
public:
  typedef typename T::myT anotherT; // ERROR - undefined type

  anotherT t; // ERROR - undefined type 

  A(anotherT _t) { // ERROR - undefined type
    t=_t;
  }

  anotherT getT() {
    return t;
  }
};

class B : public A< B > // ERROR - forward declaration
{
public:
  typedef int myT;
};

int main() {
  B b;
}
