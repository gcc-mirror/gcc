// PR c++/77775
// { dg-options "-fdump-tree-fre1 -O1" }
// { dg-final { scan-tree-dump "== viewAdded" "fre1" { target c++11 } } }

namespace Sublime {
struct View;
struct AreaIndex;
struct Area {
  void qt_static_metacall();
  void viewAdded(AreaIndex *, View *);
};
}
void Sublime::Area::qt_static_metacall() {
  typedef void (Area::*_t)(AreaIndex *, View *);
  if (*reinterpret_cast<_t *>(1) == _t(&Area::viewAdded))
    __builtin_abort();
}
