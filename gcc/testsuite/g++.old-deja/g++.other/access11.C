// Build don't link:
// Origin: r.spatschek@fz-juelich.de
// Special g++ Options: -w

class A
{
private:
  template <class T> void g(T t)  {}
  int i;
};

template <>
void A::g<int>(int t) { i = 1; } // ERROR - private

int main()
{
  A a;
 
  a.g<int>(0); // ERROR - private
}
