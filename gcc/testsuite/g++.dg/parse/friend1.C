namespace N {
  template <typename T>
  static void f ();

  struct S {
    friend void N::f<int> ();
    static void f (int);
  };
}
