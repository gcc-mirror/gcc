// Origin PR c++/51191
// { dg-options "-std=c++0x" }

template< class T >
class ClassTemplate {};

template< class T >
struct Metafunction {
  typedef T type;
};

template< class T >
using TemplateAlias = ClassTemplate< typename Metafunction<T>::type >;

using Alias = TemplateAlias<int>;

template< class T >
void f( TemplateAlias<T> );

int main()
{
  Alias x;
  f( x ); // { dg-error "no matching function for call to|f" }
}
