// PR c++/95675
// { dg-do compile { target c++11 } }

struct a {};
template <typename> struct b;
template <typename bq, typename br> struct b<bq(br)> {
  decltype(bq()(br())) c;
};
struct e {
  operator a();
};
b<e (*(e))(a)> d;
