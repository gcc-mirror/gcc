// Bug: g++ was failing to destroy C<int>::a because it was using two
// different sentry variables for construction and destruction.
//
// Some targets (e.g. those with "set_board_info needs_status_wrapper 1"
// in their dejagnu baseboard description) require that the status is
// final when exit is entered (or main returns), and not "overruled" by a
// destructor calling _exit.  It's not really worth it to handle that.
// Skip if target: mmix-knuth-mmixware xtensa-*-elf*

extern "C" void _exit (int);

int r = 1;

struct A
{
  void f(){};
  A(){ ++r; }
  ~A(){ r -= 2; _exit (r); }
};

template<class T>
struct C
{
  C(){ a.f(); }
  static A a;
};

template <class T> A C<T>::a;
typedef C<int> B;

int main()
{
  C<int> c;
  return r;
}
