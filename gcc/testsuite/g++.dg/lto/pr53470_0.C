// { dg-lto-do link }
// { dg-lto-options { { -g -flto } } }

class sp_counted_base;
class shared_count {
  sp_counted_base *pi_;
public:
  template<class Y> shared_count(Y) : pi_() {}
  ~shared_count() {}
};
template<class T> struct shared_ptr {
  T element_type;
  template<class Y> shared_ptr(Y) : pn(0) {}
  shared_count pn;
};
template<class> class ECGetterBase;
template<class T> struct ExtensionCord {
  struct Holder {
    ECGetterBase<T> *getter_;
  };
  ExtensionCord() : holder_(new Holder) {}

  shared_ptr<Holder> holder_;
};
ExtensionCord<int> a;
int main() {}
