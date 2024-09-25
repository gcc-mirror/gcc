// { dg-do assemble  }
// { dg-skip-if "requires hosted libstdc++ for vector" { ! hostedlib } }
// All the pointer_to_binary_function cases used to fail because g++
// couldn't handle converting an overloaded function to a class type.
// The first one should still fail because it requires an implicit conversion
// to pointer_to_binary_function, which has an `explicit' constructor.

// { dg-prune-output "note" }
// { dg-options "-Wno-deprecated" }

#include <vector>
#include <algorithm>
#include <functional>

using namespace std;

template <class T> class Expr 
{
public :
  Expr(){}
  Expr(const T&){}
};

template <class T >
inline bool compare(const Expr<T> a, const Expr<T> b){ return true; }

int main()
{
  vector<int>	a(3);
  sort( a.begin(), a.end(),
	static_cast<bool (*)(const Expr<int>,const Expr<int>)>(compare) );
  sort( a.begin(), a.end(), compare<int> );
  sort<vector<int>::iterator,
       pointer_to_binary_function<const Expr<int>, const Expr<int>, bool> >
    ( a.begin(), a.end(), compare ); // { dg-error "" } constructor is explicit
  sort( a.begin(), a.end(),
	ptr_fun<const Expr<int>, const Expr<int>, bool> (compare) );
  sort( a.begin(), a.end(),
	ptr_fun(compare<int>) );
  sort( a.begin(), a.end(),
	pointer_to_binary_function<const Expr<int>, const Expr<int>, bool>(compare) );
  sort( a.begin(), a.end(),
	pointer_to_binary_function<const Expr<int>, const Expr<int>, bool>(compare<int>) );
  sort( a.begin(), a.end(),
	pointer_to_binary_function<const Expr<int>, const Expr<int>, bool>(compare<>) );
}

// { dg-prune-output "enable_if" }
