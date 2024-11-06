// PR c++/20420
// FIXME: These error messages aren't great, rather than multiple definition
// we should talk about conflicting declarations with the using-decl.

class B
{
protected:
  enum E { E1, E2, E3 };     // { dg-message "previous" }
  struct S { int i; E e; };  // { dg-message "previous" }
};

class D : private B
{
public:
  using B::E;
  using B::S;

private:
  enum E {};        // { dg-error "multiple definition" }
  struct S {};      // { dg-error "redefinition" }
};

template<typename T>
class BT
{
protected:
  enum E { E1, E2, E3 };
  struct S { int i; E e; };
};

template<typename T>
class DT : private BT<T>
{
public:
  using BT<T>::E;   // { dg-message "previous" "PR c++/115806" { xfail *-*-* } }
  using BT<T>::S;   // { dg-message "previous" }

private:
  enum E {};        // { dg-error "conflicts" "PR c++/115806" { xfail *-*-* } }
  struct S {};      // { dg-error "conflicts" }
};

template class DT<int>;

namespace N
{
  int i;
  enum bob {Q};
}

void
f ()
{
  using N::i;
  using N::i;       // { dg-bogus "conflicts" "See P1787 (CWG36)" }
}
