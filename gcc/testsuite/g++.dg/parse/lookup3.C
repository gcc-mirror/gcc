struct X {};

template <int>
struct Base {
    static void foo () { 
      X::NONEXISTENT (); // { dg-error "" }
    }
};

int main () {
  Base<2>::foo ();
}
