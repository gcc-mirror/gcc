// { dg-do compile }

namespace NS {  template <typename number> class C;  }

template <typename T> class X {
    template <typename N> friend class NS::C;
};

template class X<int>;
