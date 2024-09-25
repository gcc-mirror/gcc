// PR c++/70513
// { dg-do compile { target c++11 } }

template <typename T>
class D1
{
  enum A : int;
  enum D1::A : int { foo } c; // { dg-error "extra qualification not allowed" }
};

template <typename T>
class D2
{
  enum A : int;
  enum D2<T>::A : int { foo } c; // { dg-error "extra qualification not allowed" }
};

template <typename T>
class D3
{
  enum D3::A { foo } c; // { dg-error "does not name an enumeration" }
};

template <typename T>
class D4
{
  enum D4<T>::A { foo } c; // { dg-error "does not name an enumeration" }
};

template <typename T>
class D5
{
  class D6
  {
    enum D6::A { foo } c; // { dg-error "does not name an enumeration" }
  };
};

template <typename T>
class D7
{
  class D8
  {
    enum A : int;
    enum D8::A : int { foo } c; // { dg-error "extra qualification not allowed" }
  };
};
