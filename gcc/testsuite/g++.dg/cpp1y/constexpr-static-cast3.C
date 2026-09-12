// { dg-do compile { target c++14 } }

template <typename T>
struct A
{
  unsigned char a = 0;
  constexpr T &
  foo (const unsigned char &x)
  {
    a = x;
    return *static_cast <T *> (this);
  }
};

template <typename T>
struct B
{
  unsigned char b = 0;
  constexpr T &
  bar (const unsigned char &x)
  {
    b = x;
    return *static_cast <T *> (this);	// { dg-error "'B<D>' operand \\\(of dynamic type 'C<D>'\\\) is not a base class subobject of a 'D' object" }
  }
};

template <typename T>
struct C : A <C <T>>, B <T> { };
struct D : B <D> { };

constexpr auto c = C <D> ().foo (100).bar (10);
