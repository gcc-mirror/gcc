// PR c++/107163
// { dg-additional-options "-Wsequence-point" }

struct BaseType  {
  int i;
};

template< int Seq >
class DerivedType : public DerivedType< Seq - 1 > { };

template<>
class DerivedType< -1 > : public BaseType { };

int main() {
  DerivedType< 400 > d;
  d.i = 42;
  d.i = 42;
  d.i = 42;
  d.i = 42;
  d.i = 42;
  d.i = 42;
  d.i = 42;
  d.i = 42;
  d.i = 42;
  d.i = 42;
  d.i = 42;
  d.i = 42;
  d.i = 42;
  d.i = 42;
  d.i = 42;
  d.i = 42;
  d.i = 42;
  d.i = 42;
  d.i = 42;
  d.i = 42;
  return d.i;
}
