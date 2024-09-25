// PR c++/73456
// { dg-do compile { target c++17 } }
// { dg-options "-fconcepts" }

template<typename...> struct list {};

template<typename Seq>
concept Sequence = true;

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
