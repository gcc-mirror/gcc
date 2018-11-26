struct wiimote_t;
class a {
  wiimote_t *b;
  a();
};
struct wiimote_t { // { dg-lto-message "a different type is defined in another translation unit" }
  int unid; // { dg-lto-message "a field of same name but different type is defined in another translation unit" }
};
a::a() { b = __null; }
