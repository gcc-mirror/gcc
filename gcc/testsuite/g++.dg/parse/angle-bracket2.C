// { dg-do compile }
// Contributed by MattyT <mattyt-bugzilla at tpg dot com dot au>
// PR c++/14028: Parser accepts invalid unbalanced bracket.

template <int> struct A {};
template <typename TP> class B : public A<4  {};    // { dg-error "" }
