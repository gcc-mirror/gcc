/* { dg-do compile } */

/* PR c++/21087 */

/* We used to overload the template function with the built-in
   declaration, instead of replacing it as we should, and then barf at
   the using decl because of a test that none of the overload set
   members were anticipated built-ins.  */

extern "C" signed int toupper(signed int __c) throw();
namespace std
{
  template< typename a > a toupper(a,int){}
  using ::toupper;
}

int f () {
  std::toupper((signed int)'a');
}
