// Build don't link:
// GROUPS passed templates membertemplates
struct S {
  template <class T>
  void foo(T&);
};


template <class U>
void S::foo(U&)
{
}



