// Build don't link:
// Origin: Jason Merrill <jason@cygnus.com>

struct A
{
  A() : x(123) { }
  int x;
};
A a;

template<const int A::*PX>
struct B
{
  static int g() { return a.*PX; }
};


int main(int argc, char *argv[])
{
  int n = B<&A::x>::g();
}
