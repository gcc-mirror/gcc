// PR c++/58176
// { dg-do compile { target c++11 } }

// Nil
struct nil_ { constexpr nil_ () {} };
constexpr nil_ nil;

// Cons
template <class H, class T = nil_>
struct cons_ {
    using head_ = H;
    using tail_ = T;

    H head;
    T tail;

    constexpr cons_() {}
    constexpr cons_(H const &h, T const &t) : head(h), tail(t) {}
};
template <class H, class T = nil_>
constexpr cons_<H, T> cons (H const &h, T const &t = nil) { return
cons_<H,T>(h,t); }

// List
template <class... T> struct list_s;
template <class H, class... T>
struct list_s<H, T...> {
    using type = cons_<H, typename list_s<T...>::type>;
};
template <>
struct list_s<> {
    using type = nil_;
};
template <class... T>
using list_ = typename list_s<T...>::type;
constexpr nil_ list () { return nil; }
template <class H, class... T>
constexpr list_<H, T...> list (H h, T... t) { return cons(h, list(t...)); }

constexpr auto l1 = list("monkey", 123.4, cons(1, 2), nullptr);
