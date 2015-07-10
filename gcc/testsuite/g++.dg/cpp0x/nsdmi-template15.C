// PR c++/60842
// { dg-do compile { target c++11 } }

namespace N {
    class I {};
}

template <typename, typename>
class J {};

class S {
    J<int, N::I> j = J<int, N::I>{};
};
