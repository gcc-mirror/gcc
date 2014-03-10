// { dg-do assemble  }

template <class T>
struct vector {};

template<class T>
void fn(T)
{
  enum tern { H, L, X, U };

  vector<tern> ternvec; // { dg-error "" "" { target { ! c++11 } } } composed from a local type
}

template void fn(int);
