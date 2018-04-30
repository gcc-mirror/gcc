// PR c++/78489
// { dg-do compile { target c++11 } }

template <bool P, class T = void> struct enable_if { using type = T; };
template <class T> struct enable_if<false, T> {};

template <class Dummy> struct use_type { using type = int; };

template <bool Pred>
struct get_type {
    static_assert(Pred, "");
    using type = int;
};

template <bool Val,
              class      = typename enable_if<Val>::type, // Evaluation/Substitution should end here
              class ValT = typename get_type<Val>::type,  // This should not be instantiated
              typename use_type<ValT>::type = 0           // This NTTP causes ValT to be required
            >
constexpr bool test(int) { return false; }

template <bool>
constexpr bool test(long) { return true; }

static_assert(test<false>(0), ""); // should call test(long)
