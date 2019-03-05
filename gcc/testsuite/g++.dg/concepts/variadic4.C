// PR c++/73456
// { dg-do compile { target c++17 } }
// { dg-options "-fconcepts" }

template<typename...> struct list {};

template<typename Seq>
concept bool Sequence = true;

template<Sequence... Seqs>
struct zip;

template<Sequence... Seqs>
    requires requires { typename list<Seqs...>; }
// main.cpp:12:8: internal compiler error: in non_atomic_constraint_p, at cp/logic.cc:315
struct zip<Seqs...> {};

int main()
{
    zip<list<>, list<int>> {};
}
