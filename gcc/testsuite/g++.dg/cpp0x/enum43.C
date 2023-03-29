// PR c++/101869

enum E { A };
E operator & (E e)
{
  return e;
}
E f(void)
{
    return &E::A;	 // { dg-error "not a class" "" { target c++98_only } }
}
