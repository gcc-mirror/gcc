template <typename T>
struct set {
  void insert (const T&);
  template <class X>
  void insert  (X, X);
};

struct C : public set<int> {
  void f (const int i) {
    insert (i);
  }
};
