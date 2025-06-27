// C++26 P3533R2 - constexpr virtual inheritance
// { dg-do compile { target c++26 } }

#define M(N, P1, P2, P3, P4, P5, P6, N1, N2, N3) \
struct S##N {								\
  int a, b;								\
  constexpr S##N () : a (0), b (0) {}					\
  constexpr virtual int bar (int) { return 0; }				\
};									\
struct T##N : virtual P1 S##N {						\
  int c, d;								\
  constexpr T##N () : c (0), d (0) {}					\
};									\
struct U##N : virtual P2 S##N, virtual P3 T##N {			\
  int e;								\
  constexpr U##N () : e (0) {}						\
};									\
struct V##N : virtual P4 S##N, virtual P5 T##N, virtual P6 U##N {	\
  int f;								\
  constexpr V##N () : f (0) {}						\
  constexpr const S##N *foo () const { return (const S##N *)this; }	\
};									\
constexpr V##N v##N;							\
static_assert (N1 !!dynamic_cast<const V##N *> (v##N.foo ()));		\
static_assert (N2 !!dynamic_cast<const T##N *> (v##N.foo ()));		\
static_assert (N3 !!dynamic_cast<const U##N *> (v##N.foo ()));

M(0, public, public, public, public, public, public, , , )
M(1, private, public, public, public, public, public, , , )
M(2, public, private, public, public, public, public, , , )
M(3, private, private, public, public, public, public, , , )
M(4, public, public, private, public, public, public, , , )
M(5, private, public, private, public, public, public, , , )
M(6, public, private, private, public, public, public, , , )
M(7, private, private, private, public, public, public, , , )
M(8, public, public, public, private, public, public, , , )
M(9, private, public, public, private, public, public, , , )
M(10, public, private, public, private, public, public, , , )
M(11, private, private, public, private, public, public, !, !, !)
M(12, public, public, private, private, public, public, , , )
M(13, private, public, private, private, public, public, , , )
M(14, public, private, private, private, public, public, , , )
M(15, private, private, private, private, public, public, !, !, !)
M(16, public, public, public, public, private, public, , , )
M(17, private, public, public, public, private, public, , , )
M(18, public, private, public, public, private, public, , , )
M(19, private, private, public, public, private, public, , , )
M(20, public, public, private, public, private, public, , !, )
M(21, private, public, private, public, private, public, , !, )
M(22, public, private, private, public, private, public, , !, )
M(23, private, private, private, public, private, public, , !, )
M(24, public, public, public, private, private, public, , , )
M(25, private, public, public, private, private, public, , , )
M(26, public, private, public, private, private, public, , , )
M(27, private, private, public, private, private, public, !, !, !)
M(28, public, public, private, private, private, public, , !, )
M(29, private, public, private, private, private, public, , !, )
M(30, public, private, private, private, private, public, !, !, !)
M(31, private, private, private, private, private, public, !, !, !)
M(32, public, public, public, public, public, private, , , !)
M(33, private, public, public, public, public, private, , , !)
M(34, public, private, public, public, public, private, , , !)
M(35, private, private, public, public, public, private, , , !)
M(36, public, public, private, public, public, private, , , !)
M(37, private, public, private, public, public, private, , , !)
M(38, public, private, private, public, public, private, , , !)
M(39, private, private, private, public, public, private, , , !)
M(40, public, public, public, private, public, private, , , !)
M(41, private, public, public, private, public, private, !, !, !)
M(42, public, private, public, private, public, private, , , !)
M(43, private, private, public, private, public, private, !, !, !)
M(44, public, public, private, private, public, private, , , !)
M(45, private, public, private, private, public, private, !, !, !)
M(46, public, private, private, private, public, private, , , !)
M(47, private, private, private, private, public, private, !, !, !)
M(48, public, public, public, public, private, private, , !, !)
M(49, private, public, public, public, private, private, , !, !)
M(50, public, private, public, public, private, private, , !, !)
M(51, private, private, public, public, private, private, , !, !)
M(52, public, public, private, public, private, private, , !, !)
M(53, private, public, private, public, private, private, , !, !)
M(54, public, private, private, public, private, private, , !, !)
M(55, private, private, private, public, private, private, , !, !)
M(56, public, public, public, private, private, private, !, !, !)
M(57, private, public, public, private, private, private, !, !, !)
M(58, public, private, public, private, private, private, !, !, !)
M(59, private, private, public, private, private, private, !, !, !)
M(60, public, public, private, private, private, private, !, !, !)
M(61, private, public, private, private, private, private, !, !, !)
M(62, public, private, private, private, private, private, !, !, !)
M(63, private, private, private, private, private, private, !, !, !)
