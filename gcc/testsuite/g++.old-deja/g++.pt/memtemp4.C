// { dg-do assemble  }
// GROUPS passed templates membertemplates
struct S {
  template <class T>
  void operator+(T);
};


template <class T>
void S::operator+(T)
{
}
