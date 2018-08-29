// PR c++/81204

namespace std {
  template<typename, typename> struct set { };
}
using namespace std;

template <int I, typename Result>
inline void set(Result & res)
{
    res.template set<I>();
}
