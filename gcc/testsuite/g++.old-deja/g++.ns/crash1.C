// Build don't link:

template <class T> 
struct S1 {};

namespace N {
}

struct S2 
{
  typedef N::S1<int> S2_T; // ERROR - parse error
};
