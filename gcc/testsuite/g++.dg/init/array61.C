// PR c++/92385
// { dg-do compile { target c++11 } }
// { dg-additional-options -fdump-tree-gimple }
// { dg-final { scan-tree-dump-times "item::item" 1 "gimple" { target { ! *-*-darwin* } } } }
// { dg-final { scan-tree-dump-times "item::item" 2 "gimple" { target { *-*-darwin* } } } }

struct item {
  int i;
  item();
};

struct item_array {
  item a[10];
  item_array();
};

item_array::item_array() : a{} {}
