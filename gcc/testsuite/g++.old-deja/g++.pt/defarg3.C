// Build don't link:

template <class T> 
struct S;

template <class T = int>
struct S {};

template <class T> 
struct S;

void f()
{
  S<> s;
}

