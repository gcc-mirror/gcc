namespace Glib {
  template <typename> class Value {};
  template <>         class Glib::Value<int> {}; // { dg-error "" }
}
