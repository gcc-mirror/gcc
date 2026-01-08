// PR c++/123393
// { dg-additional-options "-fmodules" }
// { dg-module-cmi fmt }

export module fmt;
namespace fmt {
  export template <typename T> void format(T);
  namespace {}
}
