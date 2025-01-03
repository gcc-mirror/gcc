/* { dg-do compile { target c++17 } } */
/* { dg-options "-Og -w" } */

struct {
  template <typename T> T *operator()(T);
} hb_addressof;
template <typename, typename, typename... Ts> static int _hb_cmp_method(Ts...) {
  return 0;
}
template <typename V, typename K>
inline bool hb_bsearch_impl(K key, V, int compar(void *, void *)) {
  V p;
  int __trans_tmp_2 = *hb_addressof(key) = compar(&__trans_tmp_2, p);
  return false;
}
struct hb_array_t {
  int arrayZ;
};
struct hb_sorted_array_t : hb_array_t {
  void bsearch() {
    hb_bsearch_impl(bsearch_impl_x, &arrayZ, _hb_cmp_method<int, int>);
  }
  int bsearch_impl_x;
};
template <typename Returned> struct hb_lazy_loader_t {
  Returned *operator->();
};
namespace OT {
struct COLR;
}
struct hb_ot_face_t {
  hb_lazy_loader_t<OT::COLR> COLR;
};
struct {
  hb_ot_face_t table;
} hb_ot_color_glyph_get_layers_face;
namespace OT {
struct IntType {
  typedef int wide_type;
  operator wide_type();
};
struct UnsizedArrayOf;
struct OffsetTo {
  template <typename Base> friend UnsizedArrayOf operator+(Base, OffsetTo);
};
template <typename, bool> using LOffsetTo = OffsetTo;
template <typename Type> using LNNOffsetTo = LOffsetTo<Type, false>;
struct UnsizedArrayOf {
  void as_array(int);
};
struct SortedUnsizedArrayOf {
  hb_sorted_array_t __trans_tmp_4;
  void bsearch() { __trans_tmp_4.bsearch(); }
};
struct COLR {
  SortedUnsizedArrayOf __trans_tmp_3;
  int get_glyph_layers() {
    __trans_tmp_3.bsearch();
    int __trans_tmp_3 = numLayers;
    (this + layersZ).as_array(__trans_tmp_3);
  }
  LNNOffsetTo<int> layersZ;
  IntType numLayers;
};
} // namespace OT
void hb_ot_color_glyph_get_layers() {
  hb_ot_color_glyph_get_layers_face.table.COLR->get_glyph_layers();
}
