//Build don't link:
#include <vector>
#include <algorithm>

template <class T> class Expr
{
public :
Expr(){};
Expr(const T&){};
};

template <class T >
inline bool compare(const Expr<T> a, const Expr<T> b){ return true; };

int main()
{
  std::vector<int>     a(3);
  std::sort( a.begin(), a.end(), compare ); // ERROR - no matching function 
}
