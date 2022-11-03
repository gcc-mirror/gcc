// PR c++/73456
// { dg-do compile { target c++17_only } }
// { dg-options "-fconcepts-ts" }

template<typename...> struct list {};

template<typename Seq>
concept bool Sequence = true;

template<Sequence... Seqs> // requires (Sequence<Seqs> && ...)
struct zip;

template<Sequence... Seqs>
    requires requires { typename list<Seqs...>; } // && (Sequence<Seqs> && ...)
struct zip<Seqs...> {}; // { dg-error "does not specialize" }
// The constraints of the specialization and the sequence are not
// comparable; the specializations are unordered.

int main()
{
    zip<list<>, list<int>> {};
}
