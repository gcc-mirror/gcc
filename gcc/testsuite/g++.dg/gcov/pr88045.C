// PR gcov-profile/88045
// { dg-options "-fprofile-arcs -ftest-coverage -std=c++11" }
// { dg-do run { target native } }
/* { dg-skip-if "requires hosted libstdc++ for vector" { ! hostedlib } } */

#include <numeric>
#include <vector>
#include <stdlib.h>

struct Foo {
    size_t size() const { return n; };
    const size_t n;
    explicit Foo(size_t a_n) : n{a_n} {};
};

template<template<typename...> class C, typename Head, typename... Tail>
struct make_with_tail {
    using type = C<Tail...>;
};

template<template<typename...> class C, typename T, typename Head, typename... Tail>
struct make_with_tail_1 {
using type = C<T, Tail...>;
};

template<typename Head, typename... Tail>
struct head {
    using type = Head;
};
template<typename... Ts>
struct Tree {
    using root_type = typename head<Ts...>::type;
    using branch_type = typename make_with_tail<Tree, Ts...>::type;
    Tree(root_type a_root, std::vector<branch_type> a_branches) :
        root{std::move(a_root)},
        branches{std::move(a_branches)}
    {
    }

    explicit Tree(root_type a_root) : root{std::move(a_root)}, branches{root.size()}
    {
    }

    root_type                root;
    std::vector<branch_type> branches;
};

template<>
struct Tree<> {
};

template<typename... Axes>
size_t size(const Tree<Axes...>& tree)
{
    return std::accumulate(
        tree.branches.begin(),
        tree.branches.end(),
        0,
        [](const size_t& count, const typename make_with_tail<Tree, Axes...>::type& branch) {
            return count + size(branch);
        });
}

template<>
inline size_t size(const Tree<>& /* empty tree */)
{
    return 1;
}

int main(int argc, char *argv[])
{
    size(Tree<Foo, Foo, Foo>{Foo{4}, {Tree<Foo, Foo>{Foo{2}, {Tree<Foo>{Foo{205}},
                                                              Tree<Foo>{Foo{261}}}},
                                      Tree<Foo, Foo>{Foo{4}, {Tree<Foo>{Foo{875}},
                                                              Tree<Foo>{Foo{492}},
                                                              Tree<Foo>{Foo{398}},
                                                              Tree<Foo>{Foo{302}}}},
                                      Tree<Foo, Foo>{Foo{6}, {Tree<Foo>{Foo{111}},
                                                              Tree<Foo>{Foo{436}},
                                                              Tree<Foo>{Foo{388}},
                                                              Tree<Foo>{Foo{879}},
                                                              Tree<Foo>{Foo{783}},
                                                              Tree<Foo>{Foo{735}}}},
                                      Tree<Foo, Foo>{Foo{3}, {Tree<Foo>{Foo{791}},
                                                              Tree<Foo>{Foo{  5}},
                                                              Tree<Foo>{Foo{841}}}}}});

    return 0;
}

// { dg-final { run-gcov pr88045.C } }
