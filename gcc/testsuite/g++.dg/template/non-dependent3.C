//PR c++/11071
// Used to ICE
// Origin: bangerth@dealii.org and rwgk@yahoo.com

template <bool b> struct X {
    template <typename T>
    static int* execute(T* x) { return x; }
};

template <typename T> void foo() {
  static bool const same = true;
  X<same>::execute ((int*)0);
}

template void foo<int> ();
