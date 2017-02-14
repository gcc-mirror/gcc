// PR c++/77775
// { dg-options -fdump-tree-gimple }
// { dg-final { scan-tree-dump "== viewAdded" "gimple" { target c++11 } } }

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
