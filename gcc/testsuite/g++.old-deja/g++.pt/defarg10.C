// { dg-do assemble  }
// Origin: Ian Nixon <ian@tharas.com>

struct A {};

template<class M, class T = A, class C> class Tc {}; // { dg-error "" } no defarg

int main ()
{
  Tc<int> oops; // { dg-error "" } using template
}
