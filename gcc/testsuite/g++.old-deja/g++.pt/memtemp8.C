// { dg-do assemble  }
// GROUPS passed templates membertemplates
struct S {
  template <class T>
  operator T();
};


template <class T>
S::operator T()
{
}

