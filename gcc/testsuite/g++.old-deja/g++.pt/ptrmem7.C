// Build don't link:
// Origin: Jason Merrill <jason@cygnus.com>

struct A {
  int i;
};

template<const int A::* P>
struct B { };

int main()
{
  B<&A::i> b;
}
