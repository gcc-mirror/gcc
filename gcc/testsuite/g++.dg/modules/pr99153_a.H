// { dg-additional-options -fmodule-header }
// { dg-module-cmi {} }

template<typename _T1>
struct pair
{
  inline void Frob ();
};

template<typename _T2>
inline void Widget ();
