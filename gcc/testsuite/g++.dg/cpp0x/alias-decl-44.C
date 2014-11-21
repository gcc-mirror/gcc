// PR c++/63849
// { dg-do compile { target c++11 } }

template <class _T, class...>
using First = _T;            // we should not use this
                             // alias with only
                             // one pack parameter (?)

template <template <class...> class _Successor,
          int,
          class... _Xs>
struct Overlay
{
    using O = _Successor<_Xs...>;
};

template <class... _Pack>
struct List
{
    template <int _s>
    using O = typename Overlay<List, _s, _Pack...>::O;

    template <template <class...> class _S>
    using Pass = _S<_Pack...>;

    template <int _i>
    using At = typename O<_i>
    ::template Pass<First>;
};

template <int _i>
using At = typename List<int, char>
::template At<_i>;

template <int _i>
void func_crash(At<_i>&) {}

int main(int argc, char *argv[])
{
    char ccc;
    int iii;
    func_crash<0>(iii);
}
