// Build don't link:
// Origin: Ian Nixon <ian@tharas.com>

struct A {};

template<class M, class T = A, class C> class Tc {}; // ERROR - no defarg

int main ()
{
  Tc<int> oops; // ERROR - using template
}
