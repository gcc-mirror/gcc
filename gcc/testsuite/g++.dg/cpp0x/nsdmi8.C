// PR c++/55670
// { dg-do compile { target c++11 } }

template <class T> using F = T;
struct X {
    F<void ()>* fp = nullptr;
};
int main () { return 0; }
