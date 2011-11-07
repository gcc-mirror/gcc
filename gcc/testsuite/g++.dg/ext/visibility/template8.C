// PR c++/35688
// { dg-require-visibility "" }
// { dg-options "-fvisibility=hidden" }

// { dg-final { scan-hidden "_Z1gI1BEvT_" } }
// { dg-final { scan-hidden "_Z1gI1AI1BEEvT_" } }

// Test that template argument visibility takes priority even over an
// explicit visibility attribute on a template.

template <class T>
struct __attribute ((visibility ("default"))) A { };
template <class T>
void g(T) __attribute ((visibility ("default")));

struct B { };

template <class T>
void g(T)
{ }

int main()
{
  g(B());
  g(A<B>());
}
