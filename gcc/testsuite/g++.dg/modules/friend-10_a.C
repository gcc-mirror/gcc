// PR c++/122646
// { dg-additional-options "-fmodules -fconcepts" }
// { dg-module-cmi M }

export module M;

template <typename T>
struct zip_view_iterator {
  void operator-(int) {}
  friend void operator-(zip_view_iterator, zip_view_iterator)
    requires requires(T x) { x.begin() - x.begin(); }
  {}
};

struct ref_view {
  ref_view begin();
};

export template <typename X> void foo() {
  zip_view_iterator<ref_view>{} - X();
}
