#define TEMPLATE 1
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
vector<int>     a(3);
sort( a.begin(), a.end(), compare );
}
