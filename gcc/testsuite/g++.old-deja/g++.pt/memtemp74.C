// Build don't link:

template <class T>
class S
{
protected:
  template <class U>
  void f(U); // ERROR - is protected

private:
  template <class U>
  void g(U); // ERROR - is private
};


void f()
{
  S<double> s;
  s.f(3); // ERROR - within this context
  s.g(2.0); // ERROR - within this context
}
