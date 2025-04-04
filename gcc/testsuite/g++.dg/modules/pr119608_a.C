// PR c++/119608
// { dg-additional-options "-fmodules" }
// { dg-module-cmi repro }

export module repro;

export template<class Visitor>
auto
visit(
  Visitor v
) -> decltype(
  v);

export template<class Visitor> auto visit(Visitor v) -> decltype(v) {
  return {};
}
