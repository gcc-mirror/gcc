// Build don't link:
// GROUPS passed templates membertemplates
struct S {
  template <class T, class U>
  S(T, U, T);
};


template <class T, class U>
S::S(T, U, T)
{
}

