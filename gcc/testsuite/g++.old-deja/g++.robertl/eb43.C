// excess errors test - XFAIL *-*-*

#include <vector.h>
#include <algo.h>

template <class T> class Expr 
{
public :
  Expr(){};
  Expr(const T&){};
};

template <class T >
inline bool compare(const Expr<T> a, const Expr<T> b){ return true; };

void main()
{
  vector<int>	a(3);
  sort( a.begin(), a.end(),
	static_cast<bool (*)(const Expr<int>,const Expr<int>)>(compare) );
  sort( a.begin(), a.end(), compare<int> );
  sort<vector<int>::iterator,
       pointer_to_binary_function<const Expr<int>, const Expr<int>, bool> >
    ( a.begin(), a.end(), compare );
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
