#define INC_FUNCTIONAL  1
#define USE_STATIC_CAST 1

#include <vector>
#include <numeric>
#ifdef INC_FUNCTIONAL
#include <functional>
#endif

using namespace std;

template<class R> int p( int val, R& r )
{
   return val + r;
}

template<class R> void f( vector<R>& v )
{
#ifdef USE_STATIC_CAST
   accumulate( v.begin(), v.end(), 0, static_cast<int (*)(int, R&)>(p) );
#else
   accumulate( v.begin(), v.end(), 0, p<R> );
#endif
}

int main()
{
   vector<int> r;
   f( r );
}
