// { dg-do compile }
// { dg-additional-options "-Wno-return-type" }

extern "C" class WvFastString;
typedef WvFastString& WvStringParm;
struct WvFastString {
  ~WvFastString();
  operator char* () {}
};
class WvString : WvFastString {};
class WvAddr {};
class WvIPAddr : WvAddr {};
struct WvIPNet : WvIPAddr {
  bool is_default() {}
};
template<class T, bool> struct WvTraits_Helper {
  static void release(T *obj) {
    delete obj;
  }
};
template<class From> struct WvTraits {
  static void release(From *obj) {
    WvTraits_Helper<From, 0>::release(obj);
  }
};
struct WvLink {
  void   *data;
  WvLink *next;
  bool    autofree;
  WvLink(bool, int) : autofree() {}
  bool get_autofree() {}

  void unlink() {
    delete this;
  }
};
struct WvListBase {
  WvLink head, *tail;
  WvListBase() : head(0, 0) {}
};
template<class T> struct WvList : WvListBase {
  ~WvList() {
    zap();
  }

  void zap(bool destroy = 1) {
    while (head.next) unlink_after(&head, destroy);
  }

  void unlink_after(WvLink *after, bool destroy) {
    WvLink *next = 0;
    T *obj       = (destroy && next->get_autofree()) ? 
                   static_cast<T*>(next->data) : 0;

    if (tail) tail = after;
    next->unlink();
    WvTraits<T>::release(obj);
  }
};
typedef WvList<WvString>WvStringListBase;
class WvStringList : WvStringListBase {};
class WvSubProc {
  WvStringList last_args, env;
};
void addroute(WvIPNet& dest, WvStringParm table) {
  if (dest.is_default() || (table != "default")) WvSubProc checkProc;
}
