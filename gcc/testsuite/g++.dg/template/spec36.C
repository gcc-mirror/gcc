/* PR c++/38089 */
/* { dg-do compile } */

struct basic_string
{
  basic_string(const int __s);
};
namespace MyNS {
  class MyClass {
    template <typename T>
    T test() { } /* { dg-message "from definition" "" { target c++98_only } } */
  };
}
template <>
basic_string MyNS::MyClass::test() /* { dg-error "specialization of" "" { target c++98_only } }*/
{ return 1; }
