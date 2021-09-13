// PR 99480 ICE on instantiation definition
// { dg-additional-options -fmodule-header }
// { dg-module-cmi {} }
template<typename _Tp>
struct atomic;

template<typename _Tp>
struct atomic<_Tp*>;


