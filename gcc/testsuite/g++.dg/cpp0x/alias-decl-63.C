// PR c++/84785
// { dg-do compile { target c++11 } }

template <typename> struct A;
template <bool> struct B;
template <bool B, typename> using enable_if_t = typename B<B>::type;
template <long> using type_pack_element = int;
struct variant {
  variant() {}
  template <typename Arg, long I = Arg::type::value,
            typename = type_pack_element<I>, enable_if_t<A<Arg>::value, int>>
  variant(Arg &&);
};

struct S {
  variant var;
};
int main() { S s; }
