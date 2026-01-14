// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

using info = decltype(^^void);

template<info R>
void
g ()
{
  int i0 = typename [:^^int:](42);
  int i1 = (typename [:^^int:])(42);
  int i2 = typename [:R:](42);
  int i3 = (typename [:R:]) 42;
}

template void g<^^int>();

struct X { X(int); operator int(); };

template<info R>
void
f ()
{
  X x1 = typename [:R:](42);
  X x2 = (typename [:R:]) 42;
  int i1 = (typename [:R:]) x1;
  int i2 = typename [:R:](x1);
}

template void f<^^X>();
