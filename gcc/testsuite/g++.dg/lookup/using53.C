// PR c++/20420

class B
{
protected:
  enum E { E1, E2, E3 };
  struct S { int i; E e; };
};

class D : private B
{
public:
  using B::E;       // { dg-message "previous" }
  using B::S;       // { dg-message "previous" }

private:
  enum E {};        // { dg-error "conflicts" }
  struct S {};      // { dg-error "conflicts" }
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
  using BT<T>::E;   // { dg-message "previous" }
  using BT<T>::S;   // { dg-message "previous" }

private:
  enum E {};        // { dg-error "conflicts" }
  struct S {};      // { dg-error "conflicts" }
};

template class DT<int>;

namespace N
{
  int i;
}

void
f ()
{
  using N::i;
  using N::i;       // { dg-error "declared" }
}
