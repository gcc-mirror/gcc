// PR c++/79470
// { dg-do compile { target c++11 } }

    template < const int&... > struct AA;

    template < > struct AA<> { };

    template < const int& II, const int&... Is >
    struct AA<II,Is...> { };

