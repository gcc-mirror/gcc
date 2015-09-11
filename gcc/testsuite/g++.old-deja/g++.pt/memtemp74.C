// { dg-do assemble  }

template <class T>
class S
{
protected:
  template <class U>
  void f(U); // { dg-message "" } protected

private:
  template <class U>
  void g(U); // { dg-message "" } private
};


void f()
{
  S<double> s;
  s.f(3); // { dg-error "" } within this context
  s.g(2.0); // { dg-error "" } within this context
}
