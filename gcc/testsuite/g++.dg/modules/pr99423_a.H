// PR 99423 ICE seeing GC freed entities
// { dg-additional-options -fmodule-header }
// { dg-module-cmi {} }
template<typename _T1>
struct pair
{
  pair() { }
};

inline pair<bool> blob ()
{
  return {};
}
