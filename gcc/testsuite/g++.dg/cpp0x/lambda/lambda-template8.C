// PR c++/55680
// { dg-do compile { target c++11 } }

template <class T> struct X {
    static void (* code ) ();
};
template <> void (* X<int>::code ) () = [](){};
