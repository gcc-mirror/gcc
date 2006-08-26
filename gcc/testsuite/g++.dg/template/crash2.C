// { dg-options "" }

template <class EnumType>
class A
{
public:
  static const EnumType size = max; // { dg-error "" }
  int table[size]; // { dg-error "constant" }
};
template <class EnumType>
const EnumType A<EnumType>::size;
 
 
namespace N
{
enum E { max = 5 };
 
struct B
{
  A<E> a;
};
 
}
 
int
main()
{
  N::B b;
 
  return 0;
}
