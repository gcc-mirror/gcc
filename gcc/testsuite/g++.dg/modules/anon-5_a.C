// PR c++/126209
// { dg-additional-options -fmodules }
// { dg-module-cmi }
export module kernel:event;
namespace { int anchor = 0; }
inline int i = anchor;
