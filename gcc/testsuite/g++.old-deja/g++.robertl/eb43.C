#include <vector.h>
#include <algo.h>

template <class T> class Expr 
{
public :
Expr(){};
Expr(const T&){};
};

#ifdef TEMPLATE
template <class T >
inline bool compare(const Expr<T> a, const Expr<T> b){ return true; };
#else
inline bool compare(const Expr<int> a, const Expr<int> b){ return true; };
#endif

void main()
{
  vector<int>	a(3);
#if TEMPLATE == 1
  sort( a.begin(), a.end(),
	static_cast<bool (*)(const Expr<int>,const Expr<int>)>(compare) );
#elif TEMPLATE == 2
  sort( a.begin(), a.end(), compare<int> );
#elif TEMPLATE == 3
  sort<vector<int>::iterator,
       pointer_to_binary_function<const Expr<int>, const Expr<int>, bool> >
    ( a.begin(), a.end(), compare );
#elif TEMPLATE == 4
  sort( a.begin(), a.end(),
	ptr_fun<const Expr<int>, const Expr<int>, bool> (compare) );
#elif TEMPLATE == 5
  sort( a.begin(), a.end(),
	ptr_fun(compare<int>) );
#elif TEMPLATE == 6
  sort( a.begin(), a.end(),
	pointer_to_binary_function<const Expr<int>, const Expr<int>, bool>(compare) );
#elif TEMPLATE == 7
  sort( a.begin(), a.end(),
	pointer_to_binary_function<const Expr<int>, const Expr<int>, bool>(compare<int>) );
#elif TEMPLATE == 8
  sort( a.begin(), a.end(),
	pointer_to_binary_function<const Expr<int>, const Expr<int>, bool>(compare<>) );
#else
  sort( a.begin(), a.end(), compare );
#endif
}
