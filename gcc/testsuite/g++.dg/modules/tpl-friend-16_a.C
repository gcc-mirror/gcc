// PR c++/115801
// { dg-additional-options "-fmodules-ts -Wno-global-module" }
// { dg-module-cmi test }

module;

template <typename T> struct GMF;
template <typename T> struct GMF_Hidden {
  int go() { GMF<T> gmf; return gmf.x; }
};

template <typename T> struct GMF {
private:
  template <typename> friend struct ::GMF_Hidden;
  int x = 1;
};

template <typename T> int test_gmf() {
  GMF_Hidden<T> h; return h.go();
}

export module test;

export using ::GMF;
export using ::test_gmf;

export template <typename> struct Attached;
template <typename T> struct Attached_Hidden {
  int go() { Attached<T> attached; return attached.x; }
};

template <typename T> struct Attached {
private:
  template <typename> friend struct ::Attached_Hidden;
  int x = 2;
};

export template <typename T> int test_attached() {
  Attached_Hidden<T> h; return h.go();
}
