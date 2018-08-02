/* This file contains templated tests that are then instantiated in
   multiple plugin tests, in order to reduce the size of each test.  */

#define ASSERT_FALSE(X) gcc_assert (!(X))
#define ASSERT_TRUE(X) gcc_assert (X)
#define ASSERT_EQ(X, Y) gcc_assert ((X) == (Y))
#define ASSERT_KNOWN_EQ(X, Y) gcc_assert (known_eq (X, Y))
#define ASSERT_MAYBE_NE(X, Y) gcc_assert (maybe_ne (X, Y))

/* make (X) converts an X of type int into T, using an arbitrary
   precision for wide_int.  It passes other types of X through as-is.  */
template<typename T>
struct coeff_helper
{
  static inline const T &make (const T &x) { return x; }
};

template<>
struct coeff_helper<wide_int>
{
  template<typename T>
  static inline const T &make (const T &x) { return x; }
  static inline wide_int make (int i) { return wi::shwi (i, 77); }
};

/* make (C1, C2, C3) constructs a T using coefficients from C1, C2 and C3,
   picking only enough to fill the T.  */
template<typename T>
struct poly_helper
{
  typedef typename poly_int_traits<T>::coeff_type C;
  template<typename T1, typename T2, typename T3>
  static T make (const T1 &a, const T2 &b, const T3 &c);
};

template<typename T>
template<typename T1, typename T2, typename T3>
inline T
poly_helper<T>::make (const T1 &a, const T2 &b, const T3 &c)
{
  T res;
  res = coeff_helper<C>::make (a);
  if (poly_int_traits<T>::num_coeffs >= 2)
    res.coeffs[1] = coeff_helper<C>::make (b);
  if (poly_int_traits<T>::num_coeffs >= 3)
    res.coeffs[2] = coeff_helper<C>::make (c);
  return res;
}

/* Test the helper,  */

static void
test_helper ()
{
  typedef poly_helper< poly_int<1, int> > p1;
  typedef poly_helper< poly_int<2, int> > p2;
  typedef poly_helper< poly_int<3, int> > p3;

  ASSERT_MAYBE_NE (p1::make (1, 2, 3), 0);
  ASSERT_KNOWN_EQ (p1::make (1, 2, 3) - p1::make (1, 0, 0), 0);
  ASSERT_KNOWN_EQ (p1::make (1, 2, 3) - p1::make (1, 2, 0), 0);
  ASSERT_KNOWN_EQ (p1::make (1, 2, 3) - p1::make (1, 2, 3), 0);

  ASSERT_MAYBE_NE (p2::make (1, 2, 3), 0);
  ASSERT_MAYBE_NE (p2::make (1, 2, 3) - p2::make (1, 0, 0), 0);
  ASSERT_KNOWN_EQ (p2::make (1, 2, 3) - p2::make (1, 2, 0), 0);
  ASSERT_KNOWN_EQ (p2::make (1, 2, 3) - p2::make (1, 2, 3), 0);

  ASSERT_MAYBE_NE (p3::make (1, 2, 3), 0);
  ASSERT_MAYBE_NE (p3::make (1, 2, 3) - p3::make (1, 0, 0), 0);
  ASSERT_MAYBE_NE (p3::make (1, 2, 3) - p3::make (1, 2, 0), 0);
  ASSERT_KNOWN_EQ (p3::make (1, 2, 3) - p3::make (1, 2, 3), 0);
}

/* Test poly_coeff_traits.  */

static void
test_poly_coeff_traits ()
{
  ASSERT_EQ (poly_coeff_traits<unsigned short>::signedness, 0);
  ASSERT_EQ (poly_coeff_traits<unsigned short>::max_value, 0xffff);

  ASSERT_EQ (poly_coeff_traits<int>::signedness, 1);
  ASSERT_EQ (poly_coeff_traits<int>::max_value, INT_MAX);

  ASSERT_EQ (poly_coeff_traits<unsigned int>::signedness, 0);
  ASSERT_EQ (poly_coeff_traits<unsigned int>::max_value, UINT_MAX);

  ASSERT_EQ (poly_coeff_traits<HOST_WIDE_INT>::signedness, 1);
  ASSERT_EQ (poly_coeff_traits<HOST_WIDE_INT>::max_value, HOST_WIDE_INT_MAX);

  ASSERT_EQ (poly_coeff_traits<unsigned HOST_WIDE_INT>::signedness, 0);
  ASSERT_EQ (poly_coeff_traits<unsigned HOST_WIDE_INT>::max_value,
	     HOST_WIDE_INT_M1U);

  ASSERT_EQ (poly_coeff_traits<wide_int>::signedness, -1);
  ASSERT_EQ (poly_coeff_traits<offset_int>::signedness, 1);
  ASSERT_EQ (poly_coeff_traits<widest_int>::signedness, 1);
}

/* Test poly_int_traits.  */

template<unsigned int N, typename C, typename T>
static void
test_poly_int_traits ()
{
  /* Check the properties of poly_int_traits<C>.  */
  ASSERT_FALSE (poly_int_traits<C>::is_poly);
  ASSERT_EQ (poly_int_traits<C>::num_coeffs, 1);
  ASSERT_EQ ((C *) 0 - (typename poly_int_traits<C>::coeff_type *) 0, 0);

  /* Check the properties of poly_int_traits<T>.  */
  ASSERT_TRUE (poly_int_traits<T>::is_poly);
  ASSERT_EQ (poly_int_traits<T>::num_coeffs, N);
  ASSERT_EQ ((C *) 0 - (typename poly_int_traits<T>::coeff_type *) 0, 0);
}

/* Test the handling of constants.  */

template<unsigned int N, typename C, typename T>
static void
test_constants ()
{
  typedef coeff_helper<C> ch;
  T zero, one, two;
  poly_int<N, unsigned char> two_uc = 2;

  /* Test operator = on C.  */
  zero = ch::make (0);
  one = ch::make (1);
  two = ch::make (2);

  /* Basic tests of known_eq and maybe_ne.  */
  ASSERT_KNOWN_EQ (zero, ch::make (0));
  ASSERT_MAYBE_NE (one, ch::make (0));
  ASSERT_MAYBE_NE (two, ch::make (0));
  ASSERT_KNOWN_EQ (ch::make (0), zero);
  ASSERT_MAYBE_NE (ch::make (0), one);
  ASSERT_MAYBE_NE (ch::make (0), two);
  ASSERT_KNOWN_EQ (zero, zero);
  ASSERT_MAYBE_NE (one, zero);
  ASSERT_MAYBE_NE (two, zero);

  ASSERT_MAYBE_NE (zero, ch::make (1));
  ASSERT_KNOWN_EQ (one, ch::make (1));
  ASSERT_MAYBE_NE (two, ch::make (1));
  ASSERT_MAYBE_NE (ch::make (1), zero);
  ASSERT_KNOWN_EQ (ch::make (1), one);
  ASSERT_MAYBE_NE (ch::make (1), two);
  ASSERT_MAYBE_NE (zero, one);
  ASSERT_KNOWN_EQ (one, one);
  ASSERT_MAYBE_NE (two, one);

  ASSERT_MAYBE_NE (zero, ch::make (2));
  ASSERT_MAYBE_NE (one, ch::make (2));
  ASSERT_KNOWN_EQ (two, ch::make (2));
  ASSERT_MAYBE_NE (ch::make (2), zero);
  ASSERT_MAYBE_NE (ch::make (2), one);
  ASSERT_KNOWN_EQ (ch::make (2), two);
  ASSERT_MAYBE_NE (zero, two);
  ASSERT_MAYBE_NE (one, two);
  ASSERT_KNOWN_EQ (two, two);

  ASSERT_MAYBE_NE (zero, two_uc);
  ASSERT_MAYBE_NE (one, two_uc);
  ASSERT_KNOWN_EQ (two, two_uc);
  ASSERT_MAYBE_NE (two_uc, zero);
  ASSERT_MAYBE_NE (two_uc, one);
  ASSERT_KNOWN_EQ (two_uc, two);
}

/* Test operator +=.  */

template<unsigned int N, typename C, typename T>
static void
test_plus_equals ()
{
  typedef poly_helper<T> ph;

  /* Test += on int.  */
  T add_cm = ph::make (17, 11, 9);
  add_cm += 14;
  ASSERT_KNOWN_EQ (add_cm, ph::make (31, 11, 9));

  /* Test += on T.  */
  T add_pm = ph::make (100, 44, 11);
  add_pm += ph::make (1, 2, 3);
  ASSERT_KNOWN_EQ (add_pm, ph::make (101, 46, 14));
}

/* Test operator -=.  */

template<unsigned int N, typename C, typename T>
static void
test_minus_equals ()
{
  typedef poly_helper<T> ph;

  /* Test -= on int.  */
  T sub_cm = ph::make (82, 13, 61);
  sub_cm -= 76;
  ASSERT_KNOWN_EQ (sub_cm, ph::make (6, 13, 61));

  /* Test -= on T.  */
  T sub_pm = ph::make (82, 13, 61);
  sub_pm -= ph::make (19, 12, 14);
  ASSERT_KNOWN_EQ (sub_pm, ph::make (63, 1, 47));
}

/* Test operator *=.  */

template<unsigned int N, typename C, typename T>
static void
test_times_equals ()
{
  typedef poly_helper<T> ph;

  /* Test *= on int.  */
  T mul_cm = ph::make (11, 22, 33);
  mul_cm *= 3;
  ASSERT_KNOWN_EQ (mul_cm, ph::make (33, 66, 99));
}

/* Test operator <<=.  */

template<unsigned int N, typename C, typename T>
static void
test_shl_equals ()
{
  typedef poly_helper<T> ph;

  /* Test <<= on int.  */
  T shl_cm = ph::make (10, 11, 13);
  shl_cm <<= 2;
  ASSERT_KNOWN_EQ (shl_cm, ph::make (40, 44, 52));
}

/* Test is_constant.  */

template<unsigned int N, typename C, typename T>
static void
test_is_constant ()
{
  typedef poly_helper<T> ph;

  /* Test is_constant without arguments.  */
  ASSERT_TRUE (ph::make (1, 0, 0).is_constant ());
  ASSERT_EQ (ph::make (2, 0, 1).is_constant (), N <= 2);
  ASSERT_EQ (ph::make (3, 1, 0).is_constant (), N == 1);

  /* Test is_constant with an argument.  */
  C const_value;
  ASSERT_TRUE (ph::make (1, 0, 0).is_constant (&const_value));
  ASSERT_EQ (const_value, 1);
  ASSERT_EQ (ph::make (2, 0, 1).is_constant (&const_value), N <= 2);
  ASSERT_EQ (const_value, N <= 2 ? 2 : 1);
  ASSERT_EQ (ph::make (3, 1, 0).is_constant (&const_value), N == 1);
  ASSERT_EQ (const_value, 4 - N);
}

/* Test to_constant.  */

template<unsigned int N, typename C, typename T>
static void
test_to_constant ()
{
  typedef poly_helper<T> ph;

  ASSERT_TRUE (ph::make (1, 0, 0).to_constant () == 1);
  ASSERT_TRUE (ph::make (111, 0, 0).to_constant () == 111);
}

/* Test addition, both via operators and wi::.  */

template<unsigned int N, typename C, typename T>
static void
test_addition ()
{
  typedef poly_helper<T> ph;

  /* Test +.  */
  ASSERT_KNOWN_EQ (ph::make (55, 43, 30) + 1,
		   ph::make (56, 43, 30));
  ASSERT_KNOWN_EQ (100 + ph::make (5, 15, 26),
		   ph::make (105, 15, 26));
  ASSERT_KNOWN_EQ (ph::make (7, 100, 41) + ph::make (96, 9, 21),
		   ph::make (103, 109, 62));

  /* Test wi::add.  */
  ASSERT_KNOWN_EQ (wi::add (ph::make (55, 43, 30), 1),
		   ph::make (56, 43, 30));
  ASSERT_KNOWN_EQ (wi::add (100, ph::make (5, 15, 26)),
		   ph::make (105, 15, 26));
  ASSERT_KNOWN_EQ (wi::add (ph::make (7, 100, 41), ph::make (96, 9, 21)),
		   ph::make (103, 109, 62));
}

/* Test subtraction, both via operators and wi::.  */

template<unsigned int N, typename C, typename RC, typename T>
static void
test_subtraction ()
{
  typedef poly_helper<T> ph;
  typedef poly_helper< poly_int<N, RC> > rph;
  typedef poly_helper< poly_int<N, int> > iph;

  /* Test -.  Cs with a rank lower than HOST_WIDE_INT promote to
     HOST_WIDE_INT; use rph to capture this.  */
  ASSERT_KNOWN_EQ (ph::make (64, 49, 36) - 42,
		   rph::make (22, 49, 36));
  ASSERT_KNOWN_EQ (11 - ph::make (9, 3, 4),
		   rph::make (2, -3, -4));
  ASSERT_KNOWN_EQ (ph::make (100, 200, 300) - ph::make (99, 197, 305),
		   rph::make (1, 3, -5));

  /* Test wi::sub.  Primitive Cs promote to widest_int; use iph to capture
     this.  */
  ASSERT_KNOWN_EQ (wi::sub (ph::make (64, 49, 36), 42),
		   iph::make (22, 49, 36));
  ASSERT_KNOWN_EQ (wi::sub (11, ph::make (9, 3, 4)),
		   iph::make (2, -3, -4));
  ASSERT_KNOWN_EQ (wi::sub (ph::make (100, 200, 300), ph::make (99, 197, 305)),
		   iph::make (1, 3, -5));
}

/* Test negation, both via operators and wi::.  */

template<unsigned int N, typename C, typename RC, typename T>
static void
test_negation ()
{
  typedef poly_helper<T> ph;
  typedef poly_helper< poly_int<N, RC> > rph;
  typedef poly_helper< poly_int<N, int> > iph;

  /* Test unary -.  */
  ASSERT_KNOWN_EQ (-ph::make (10, 20, 30),
		   rph::make (-10, -20, -30));

  /* Test wi::neg.  */
  ASSERT_KNOWN_EQ (wi::neg (ph::make (10, 20, 30)),
		   iph::make (-10, -20, -30));
}

/* Test multiplication, both via operators and wi::.  */

template<unsigned int N, typename C, typename T>
static void
test_multiplication ()
{
  typedef poly_helper<T> ph;

  /* Test *.  */
  ASSERT_KNOWN_EQ (ph::make (5, 20, 25) * 10,
		   ph::make (50, 200, 250));
  ASSERT_KNOWN_EQ (111 * ph::make (7, 6, 5),
		   ph::make (777, 666, 555));

  /* Test wi::mul.  */
  ASSERT_KNOWN_EQ (wi::mul (ph::make (5, 20, 25), 10),
		   ph::make (50, 200, 250));
  ASSERT_KNOWN_EQ (wi::mul (111, ph::make (7, 6, 5)),
		   ph::make (777, 666, 555));
}

/* Test shift left, both via operators and wi::.  */

template<unsigned int N, typename C, typename T>
static void
test_shift_left ()
{
  typedef poly_helper<T> ph;

  /* Test <<.  */
  ASSERT_KNOWN_EQ (ph::make (1, 20, 300) << 4,
		   ph::make (16, 320, 4800));

  /* Test wi::lshift.  */
  ASSERT_KNOWN_EQ (wi::lshift (ph::make (9, 15, 50), 3),
		   ph::make (72, 120, 400));
}

/* Test maybe_ne.  */

template<unsigned int N, typename C, typename T>
static void
test_maybe_ne ()
{
  typedef coeff_helper<C> ch;
  typedef poly_helper<T> ph;

  /* Test maybe_ne (T, C).  */
  ASSERT_EQ (maybe_ne (ph::make (1, 0, 2), ch::make (1)), N == 3);
  ASSERT_EQ (maybe_ne (ph::make (-11, -2, 0), ch::make (-11)), N >= 2);
  ASSERT_TRUE (maybe_ne (ph::make (199, 0, 0), ch::make (200)));

  /* Test maybe_ne (C, T).  */
  ASSERT_EQ (maybe_ne (ch::make (-22), ph::make (-22, 0, -1)), N == 3);
  ASSERT_EQ (maybe_ne (ch::make (5), ph::make (5, 4, 0)), N >= 2);
  ASSERT_TRUE (maybe_ne (ch::make (-3), ph::make (-4, 0, 0)));

  /* Test maybe_ne (T, T).  */
  ASSERT_EQ (maybe_ne (ph::make (1, 3, 5),
		       ph::make (1, 3, 6)), N == 3);
  ASSERT_EQ (maybe_ne (ph::make (1, 3, 5),
		       ph::make (1, 4, 5)), N >= 2);
  ASSERT_TRUE (maybe_ne (ph::make (1, 3, 5),
			 ph::make (0, 3, 5)));
}

/* Test known_eq.  */

template<unsigned int N, typename C, typename T>
static void
test_known_eq ()
{
  typedef coeff_helper<C> ch;
  typedef poly_helper<T> ph;

  /* Test known_eq (T, C).  */
  ASSERT_EQ (known_eq (ph::make (1, 0, 2), ch::make (1)), N <= 2);
  ASSERT_EQ (known_eq (ph::make (-11, -2, 0), ch::make (-11)), N == 1);
  ASSERT_FALSE (known_eq (ph::make (199, 0, 0), ch::make (200)));

  /* Test known_eq (C, T).  */
  ASSERT_EQ (known_eq (ch::make (-22), ph::make (-22, 0, -1)), N <= 2);
  ASSERT_EQ (known_eq (ch::make (5), ph::make (5, 4, 0)), N == 1);
  ASSERT_FALSE (known_eq (ch::make (-3), ph::make (-4, 0, 0)));

  /* Test known_eq (T, T).  */
  ASSERT_EQ (known_eq (ph::make (1, 3, 5),
		       ph::make (1, 3, 6)), N <= 2);
  ASSERT_EQ (known_eq (ph::make (1, 3, 5),
		       ph::make (1, 4, 5)), N == 1);
  ASSERT_FALSE (known_eq (ph::make (1, 3, 5),
			  ph::make (0, 3, 5)));
}

/* Test can_align_p.  */

template<unsigned int N, typename C, typename T>
static void
test_can_align_p ()
{
  typedef poly_helper<T> ph;

  ASSERT_TRUE (can_align_p (ph::make (41, 32, 16), 16));
  ASSERT_EQ (can_align_p (ph::make (15, 64, 8), 16), N <= 2);
  ASSERT_EQ (can_align_p (ph::make (17, 8, 80), 16), N == 1);
  ASSERT_TRUE (can_align_p (ph::make (-39, -64, -32), 32));
  ASSERT_EQ (can_align_p (ph::make (-32, -96, -31), 32), N <= 2);
  ASSERT_EQ (can_align_p (ph::make (-31, -31, -128), 32), N == 1);
  ASSERT_TRUE (can_align_p (ph::make (17, 0, 0), 16));
  ASSERT_TRUE (can_align_p (ph::make (16, 0, 0), 16));
  ASSERT_TRUE (can_align_p (ph::make (15, 0, 0), 16));
  ASSERT_TRUE (can_align_p (ph::make (-17, 0, 0), 16));
  ASSERT_TRUE (can_align_p (ph::make (-16, 0, 0), 16));
  ASSERT_TRUE (can_align_p (ph::make (-15, 0, 0), 16));
}

/* Test can_align_up.  */

template<unsigned int N, typename C, typename T>
static void
test_can_align_up ()
{
  typedef coeff_helper<C> ch;
  typedef poly_helper<T> ph;

  T aligned;
  ASSERT_TRUE (can_align_up (ph::make (41, 32, 16), 16, &aligned));
  ASSERT_KNOWN_EQ (aligned, ph::make (48, 32, 16));
  ASSERT_EQ (can_align_up (ph::make (15, 64, 8), 16, &aligned), N <= 2);
  if (N <= 2)
    ASSERT_KNOWN_EQ (aligned, ph::make (16, 64, 0));
  ASSERT_EQ (can_align_up (ph::make (17, 8, 80), 16, &aligned), N == 1);
  if (N == 1)
    ASSERT_KNOWN_EQ (aligned, ch::make (32));
  ASSERT_TRUE (can_align_up (ph::make (-39, -64, -32), 32, &aligned));
  ASSERT_KNOWN_EQ (aligned, ph::make (-32, -64, -32));
  ASSERT_EQ (can_align_up (ph::make (-32, -96, -31), 32, &aligned), N <= 2);
  if (N <= 2)
    ASSERT_KNOWN_EQ (aligned, ph::make (-32, -96, 0));
  ASSERT_EQ (can_align_up (ph::make (-31, -31, -128), 32, &aligned), N == 1);
  if (N == 1)
    ASSERT_KNOWN_EQ (aligned, ch::make (0));
  ASSERT_TRUE (can_align_up (ph::make (17, 0, 0), 16, &aligned));
  ASSERT_KNOWN_EQ (aligned, ch::make (32));
  ASSERT_TRUE (can_align_up (ph::make (16, 0, 0), 16, &aligned));
  ASSERT_KNOWN_EQ (aligned, ch::make (16));
  ASSERT_TRUE (can_align_up (ph::make (15, 0, 0), 16, &aligned));
  ASSERT_KNOWN_EQ (aligned, ch::make (16));
  ASSERT_TRUE (can_align_up (ph::make (-17, 0, 0), 16, &aligned));
  ASSERT_KNOWN_EQ (aligned, ch::make (-16));
  ASSERT_TRUE (can_align_up (ph::make (-16, 0, 0), 16, &aligned));
  ASSERT_KNOWN_EQ (aligned, ch::make (-16));
  ASSERT_TRUE (can_align_up (ph::make (-15, 0, 0), 16, &aligned));
  ASSERT_KNOWN_EQ (aligned, ch::make (0));
}

/* Test can_align_down.  */

template<unsigned int N, typename C, typename T>
static void
test_can_align_down ()
{
  typedef coeff_helper<C> ch;
  typedef poly_helper<T> ph;

  T aligned;
  ASSERT_TRUE (can_align_down (ph::make (41, 32, 16), 16, &aligned));
  ASSERT_KNOWN_EQ (aligned, ph::make (32, 32, 16));
  ASSERT_EQ (can_align_down (ph::make (15, 64, 8), 16, &aligned), N <= 2);
  if (N <= 2)
    ASSERT_KNOWN_EQ (aligned, ph::make (0, 64, 0));
  ASSERT_EQ (can_align_down (ph::make (17, 8, 80), 16, &aligned), N == 1);
  if (N == 1)
    ASSERT_KNOWN_EQ (aligned, ch::make (16));
  ASSERT_TRUE (can_align_down (ph::make (-39, -64, -32), 32, &aligned));
  ASSERT_KNOWN_EQ (aligned, ph::make (-64, -64, -32));
  ASSERT_EQ (can_align_down (ph::make (-32, -96, -31), 32, &aligned), N <= 2);
  if (N <= 2)
    ASSERT_KNOWN_EQ (aligned, ph::make (-32, -96, 0));
  ASSERT_EQ (can_align_down (ph::make (-31, -31, -128), 32, &aligned), N == 1);
  if (N == 1)
    ASSERT_KNOWN_EQ (aligned, ch::make (-32));
  ASSERT_TRUE (can_align_down (ph::make (17, 0, 0), 16, &aligned));
  ASSERT_KNOWN_EQ (aligned, ch::make (16));
  ASSERT_TRUE (can_align_down (ph::make (16, 0, 0), 16, &aligned));
  ASSERT_KNOWN_EQ (aligned, ch::make (16));
  ASSERT_TRUE (can_align_down (ph::make (15, 0, 0), 16, &aligned));
  ASSERT_KNOWN_EQ (aligned, ch::make (0));
  ASSERT_TRUE (can_align_down (ph::make (-17, 0, 0), 16, &aligned));
  ASSERT_KNOWN_EQ (aligned, ch::make (-32));
  ASSERT_TRUE (can_align_down (ph::make (-16, 0, 0), 16, &aligned));
  ASSERT_KNOWN_EQ (aligned, ch::make (-16));
  ASSERT_TRUE (can_align_down (ph::make (-15, 0, 0), 16, &aligned));
  ASSERT_KNOWN_EQ (aligned, ch::make (-16));
}

/* Test known_equal_after_align_up.  */

template<unsigned int N, typename C, typename T>
static void
test_known_equal_after_align_up ()
{
  typedef poly_helper<T> ph;

  ASSERT_EQ (known_equal_after_align_up (ph::make (15, 15, 32),
					 ph::make (16, 15, 32), 16), N == 1);
  ASSERT_EQ (known_equal_after_align_up (ph::make (16, 16, 15),
					 ph::make (15, 16, 15), 16), N <= 2);
  ASSERT_EQ (known_equal_after_align_up (ph::make (15, 16, 32),
					 ph::make (7, 16, 48), 16), N <= 2);
  ASSERT_EQ (known_equal_after_align_up (ph::make (7, 32, 16),
					 ph::make (15, 48, 16), 16), N == 1);
  ASSERT_TRUE (known_equal_after_align_up (ph::make (16, 16, 32),
					   ph::make (15, 16, 32), 16));
  ASSERT_TRUE (known_equal_after_align_up (ph::make (32, 0, 0),
					   ph::make (31, 0, 0), 16));
  ASSERT_TRUE (known_equal_after_align_up (ph::make (32, 0, 0),
					   ph::make (32, 0, 0), 32));
  ASSERT_FALSE (known_equal_after_align_up (ph::make (32, 0, 0),
					    ph::make (33, 0, 0), 16));
  ASSERT_FALSE (known_equal_after_align_up (ph::make (-31, 0, 0),
					    ph::make (-32, 0, 0), 16));
  ASSERT_TRUE (known_equal_after_align_up (ph::make (-32, 0, 0),
					   ph::make (-32, 0, 0), 32));
  ASSERT_TRUE (known_equal_after_align_up (ph::make (-33, 0, 0),
					   ph::make (-32, 0, 0), 16));
}

/* Test known_equal_after_align_down.  */

template<unsigned int N, typename C, typename T>
static void
test_known_equal_after_align_down ()
{
  typedef poly_helper<T> ph;

  ASSERT_EQ (known_equal_after_align_down (ph::make (17, 15, 32),
					   ph::make (16, 15, 32), 16), N == 1);
  ASSERT_EQ (known_equal_after_align_down (ph::make (16, 16, 15),
					   ph::make (17, 16, 15), 16), N <= 2);
  ASSERT_EQ (known_equal_after_align_down (ph::make (15, 16, 32),
					   ph::make (7, 16, 48), 16), N <= 2);
  ASSERT_EQ (known_equal_after_align_down (ph::make (15, 32, 16),
					   ph::make (7, 48, 16), 16), N == 1);
  ASSERT_TRUE (known_equal_after_align_down (ph::make (16, 16, 32),
					     ph::make (17, 16, 32), 16));
  ASSERT_FALSE (known_equal_after_align_down (ph::make (32, 0, 0),
					      ph::make (31, 0, 0), 16));
  ASSERT_TRUE (known_equal_after_align_down (ph::make (32, 0, 0),
					     ph::make (32, 0, 0), 32));
  ASSERT_TRUE (known_equal_after_align_down (ph::make (32, 0, 0),
					     ph::make (33, 0, 0), 16));
  ASSERT_TRUE (known_equal_after_align_down (ph::make (-31, 0, 0),
					     ph::make (-32, 0, 0), 16));
  ASSERT_TRUE (known_equal_after_align_down (ph::make (-32, 0, 0),
					     ph::make (-32, 0, 0), 32));
  ASSERT_FALSE (known_equal_after_align_down (ph::make (-33, 0, 0),
					      ph::make (-32, 0, 0), 16));
}

/* Test force_align_up.  */

template<unsigned int N, typename C, typename T>
static void
test_force_align_up ()
{
  typedef coeff_helper<C> ch;
  typedef poly_helper<T> ph;

  /* Test force_align_up.  */
  ASSERT_KNOWN_EQ (force_align_up (ph::make (41, 32, 16), 16),
		   ph::make (48, 32, 16));
  ASSERT_KNOWN_EQ (force_align_up (ph::make (-39, -64, -32), 32),
		   ph::make (-32, -64, -32));
  ASSERT_KNOWN_EQ (force_align_up (ph::make (17, 0, 0), 16),
		   ch::make (32));
  ASSERT_KNOWN_EQ (force_align_up (ph::make (16, 0, 0), 16),
		   ch::make (16));
  ASSERT_KNOWN_EQ (force_align_up (ph::make (15, 0, 0), 16),
		   ch::make (16));
  ASSERT_KNOWN_EQ (force_align_up (ph::make (-17, 0, 0), 16),
		   ch::make (-16));
  ASSERT_KNOWN_EQ (force_align_up (ph::make (-16, 0, 0), 16),
		   ch::make (-16));
  ASSERT_KNOWN_EQ (force_align_up (ph::make (-15, 0, 0), 16),
		   ch::make (0));
}

/* Test force_align_down.  */

template<unsigned int N, typename C, typename T>
static void
test_force_align_down ()
{
  typedef coeff_helper<C> ch;
  typedef poly_helper<T> ph;

  ASSERT_KNOWN_EQ (force_align_down (ph::make (41, 32, 16), 16),
		   ph::make (32, 32, 16));
  ASSERT_KNOWN_EQ (force_align_down (ph::make (-39, -64, -32), 32),
		   ph::make (-64, -64, -32));
  ASSERT_KNOWN_EQ (force_align_down (ph::make (17, 0, 0), 16),
		   ch::make (16));
  ASSERT_KNOWN_EQ (force_align_down (ph::make (16, 0, 0), 16),
		   ch::make (16));
  ASSERT_KNOWN_EQ (force_align_down (ph::make (15, 0, 0), 16),
		   ch::make (0));
  ASSERT_KNOWN_EQ (force_align_down (ph::make (-17, 0, 0), 16),
		   ch::make (-32));
  ASSERT_KNOWN_EQ (force_align_down (ph::make (-16, 0, 0), 16),
		   ch::make (-16));
  ASSERT_KNOWN_EQ (force_align_down (ph::make (-15, 0, 0), 16),
		   ch::make (-16));
}

/* Test aligned_lower_bound.  */

template<unsigned int N, typename C, typename T>
static void
test_aligned_lower_bound ()
{
  typedef poly_helper<T> ph;

  ASSERT_KNOWN_EQ (aligned_lower_bound (ph::make (17, 63, 33), 16),
		   ph::make (16, 48, 32));
  ASSERT_KNOWN_EQ (aligned_lower_bound (ph::make (11, -33, 64), 32),
		   ph::make (0, -64, 64));
  ASSERT_KNOWN_EQ (aligned_lower_bound (ph::make (-9, 16, -31), 8),
		   ph::make (-16, 16, -32));
  ASSERT_KNOWN_EQ (aligned_lower_bound (ph::make (-8, -12, 16), 4),
		   ph::make (-8, -12, 16));
}

/* Test aligned_upper_bound.  */

template<unsigned int N, typename C, typename T>
static void
test_aligned_upper_bound ()
{
  typedef poly_helper<T> ph;

  ASSERT_KNOWN_EQ (aligned_upper_bound (ph::make (17, 63, 33), 16),
		   ph::make (32, 64, 48));
  ASSERT_KNOWN_EQ (aligned_upper_bound (ph::make (11, -33, 64), 32),
		   ph::make (32, -32, 64));
  ASSERT_KNOWN_EQ (aligned_upper_bound (ph::make (-9, 16, -31), 8),
		   ph::make (-8, 16, -24));
  ASSERT_KNOWN_EQ (aligned_upper_bound (ph::make (-8, -12, 16), 4),
		   ph::make (-8, -12, 16));
}

/* Test known_misalignment.  */

template<unsigned int N, typename C, typename T>
static void
test_known_misalignment ()
{
  typedef poly_helper<T> ph;

  C misalignment;
  ASSERT_TRUE (known_misalignment (ph::make (45, 8, 24), 8, &misalignment));
  ASSERT_EQ (misalignment, 5);
  ASSERT_EQ (known_misalignment (ph::make (17, 16, 23), 8, &misalignment),
	     N <= 2);
  ASSERT_EQ (misalignment, N <= 2 ? 1 : 5);
  ASSERT_EQ (known_misalignment (ph::make (31, 15, 0), 16, &misalignment),
	     N == 1);
  ASSERT_EQ (misalignment, N == 1 ? 15 : N == 2 ? 1 : 5);
  ASSERT_TRUE (known_misalignment (ph::make (-45, -8, -24), 8, &misalignment));
  ASSERT_EQ (misalignment, 3);
  ASSERT_TRUE (known_misalignment (ph::make (-11, 0, 0), 32, &misalignment));
  ASSERT_EQ (misalignment, 21);
}

/* Test force_get_misalignment.  */

template<unsigned int N, typename C, typename T>
static void
test_force_get_misalignment ()
{
  typedef poly_helper<T> ph;

  ASSERT_EQ (force_get_misalignment (ph::make (45, 8, 24), 8), 5);
  ASSERT_EQ (force_get_misalignment (ph::make (17, 16, 24), 8), 1);
  ASSERT_EQ (force_get_misalignment (ph::make (31, -16, 0), 16), 15);
  ASSERT_EQ (force_get_misalignment (ph::make (-45, -8, -24), 8), 3);
  ASSERT_EQ (force_get_misalignment (ph::make (-11, 0, 0), 32), 21);
}

/* Test known_alignment.  */

template<unsigned int N, typename C, typename T>
static void
test_known_alignment ()
{
  typedef poly_helper<T> ph;

  ASSERT_EQ (known_alignment (ph::make (16, 24, 30)),
	     N == 1 ? 16 : N == 2 ? 8 : 2);
  ASSERT_EQ (known_alignment (ph::make (30, 0, 31)),
	     N <= 2 ? 2 : 1);
  ASSERT_EQ (known_alignment (ph::make (20, 16, 24)), 4);
  ASSERT_EQ (known_alignment (ph::make (24, 0, 0)), 8);
  ASSERT_EQ (known_alignment (ph::make (0, 0, 0)), 0);
  ASSERT_EQ (known_alignment (ph::make (0, 12, 0)),
	     N == 1 ? 0 : 4);
  ASSERT_EQ (known_alignment (ph::make (0, 12, 6)),
	     N == 1 ? 0 : N == 2 ? 4 : 2);
  ASSERT_EQ (known_alignment (ph::make (-40, -80, -12)),
	     N <= 2 ? 8 : 4);
}

/* Test can_ior_p.  */

template<unsigned int N, typename C, typename T>
static void
test_can_ior_p ()
{
  typedef coeff_helper<C> ch;
  typedef poly_helper<T> ph;

  T ior;
  ASSERT_TRUE (can_ior_p (ph::make (0x87, 0x60, 0xa0), 0x13, &ior));
  ASSERT_KNOWN_EQ (ior, ph::make (0x97, 0x60, 0xa0));
  ASSERT_EQ (can_ior_p (ph::make (9, 96, 48), 28, &ior), N <= 2);
  if (N <= 2)
    ASSERT_KNOWN_EQ (ior, ph::make (29, 96, 0));
  ASSERT_EQ (can_ior_p (ph::make (0x81, 0x20, 0), 0x44, &ior), N == 1);
  if (N == 1)
    ASSERT_KNOWN_EQ (ior, ch::make (0xc5));
}

/* Test maybe_eq for poly_int<2, C>.  */

template<typename C>
static void
test_maybe_eq_2 ()
{
  typedef poly_int<2, C> T;

  /* Test maybe_eq (T, C).  */
  ASSERT_TRUE (maybe_eq (T (1, 4), 41));
  ASSERT_FALSE (maybe_eq (T (1, 4), 42));
  ASSERT_FALSE (maybe_eq (T (1, 4), 40));
  ASSERT_TRUE (maybe_eq (T (1, 4), 1));
  ASSERT_FALSE (maybe_eq (T (1, 4), 0));
  ASSERT_FALSE (maybe_eq (T (1, 4), 2));

  /* Test maybe_eq (C, T).  */
  ASSERT_TRUE (maybe_eq (20, T (5, 3)));
  ASSERT_FALSE (maybe_eq (21, T (5, 3)));
  ASSERT_FALSE (maybe_eq (19, T (5, 3)));
  ASSERT_TRUE (maybe_eq (5, T (5, 3)));
  ASSERT_FALSE (maybe_eq (2, T (5, 3)));
  ASSERT_FALSE (maybe_eq (6, T (5, 3)));

  /* Test maybe_eq (T, T).  */
  ASSERT_TRUE (maybe_eq (T (2, 5), T (22, 3)));
  ASSERT_FALSE (maybe_eq (T (3, 5), T (22, 3)));
  ASSERT_FALSE (maybe_eq (T (2, 5), T (23, 3)));
  ASSERT_FALSE (maybe_eq (T (2, 5), T (3, 5)));
  ASSERT_TRUE (maybe_eq (T (10, 3), T (19, 0)));
  ASSERT_FALSE (maybe_eq (T (10, 3), T (20, 0)));
  ASSERT_TRUE (maybe_eq (T (10, 0), T (4, 2)));
  ASSERT_FALSE (maybe_eq (T (11, 0), T (4, 2)));
}

/* Test known_ne for poly_int<2, C>.  */

template<typename C>
static void
test_known_ne_2 ()
{
  typedef poly_int<2, C> T;

  /* Test known_ne (T, C).  */
  ASSERT_FALSE (known_ne (T (1, 4), 41));
  ASSERT_TRUE (known_ne (T (1, 4), 42));
  ASSERT_TRUE (known_ne (T (1, 4), 40));
  ASSERT_FALSE (known_ne (T (1, 4), 1));
  ASSERT_TRUE (known_ne (T (1, 4), 0));
  ASSERT_TRUE (known_ne (T (1, 4), 2));

  /* Test known_ne (C, T).  */
  ASSERT_FALSE (known_ne (20, T (5, 3)));
  ASSERT_TRUE (known_ne (21, T (5, 3)));
  ASSERT_TRUE (known_ne (19, T (5, 3)));
  ASSERT_FALSE (known_ne (5, T (5, 3)));
  ASSERT_TRUE (known_ne (2, T (5, 3)));
  ASSERT_TRUE (known_ne (6, T (5, 3)));

  /* Test known_ne (T, T).  */
  ASSERT_FALSE (known_ne (T (2, 5), T (22, 3)));
  ASSERT_TRUE (known_ne (T (3, 5), T (22, 3)));
  ASSERT_TRUE (known_ne (T (2, 5), T (23, 3)));
  ASSERT_TRUE (known_ne (T (2, 5), T (3, 5)));
  ASSERT_FALSE (known_ne (T (10, 3), T (19, 0)));
  ASSERT_TRUE (known_ne (T (10, 3), T (20, 0)));
  ASSERT_FALSE (known_ne (T (10, 0), T (4, 2)));
  ASSERT_TRUE (known_ne (T (11, 0), T (4, 2)));
}

/* Test maybe_le for both signed and unsigned C.  */

template<unsigned int N, typename C, typename T>
static void
test_maybe_le ()
{
  typedef coeff_helper<C> ch;
  typedef poly_helper<T> ph;

  /* Test maybe_le (T, C).  */
  ASSERT_FALSE (maybe_le (ph::make (7, 5, 4), ch::make (6)));
  ASSERT_FALSE (maybe_le (ph::make (7, 0, 0), ch::make (6)));
  ASSERT_TRUE (maybe_le (ph::make (60, 1, 2), ch::make (60)));
  ASSERT_TRUE (maybe_le (ph::make (60, 0, 0), ch::make (60)));
  ASSERT_TRUE (maybe_le (ph::make (30, 9, 4), ch::make (31)));
  ASSERT_TRUE (maybe_le (ph::make (30, 0, 0), ch::make (31)));

  /* Test maybe_le (C, T).  */
  ASSERT_TRUE (maybe_le (ch::make (6), ph::make (7, 5, 4)));
  ASSERT_TRUE (maybe_le (ch::make (6), ph::make (7, 0, 0)));
  ASSERT_TRUE (maybe_le (ch::make (60), ph::make (60, 1, 2)));
  ASSERT_TRUE (maybe_le (ch::make (60), ph::make (60, 0, 0)));
  ASSERT_EQ (maybe_le (ch::make (31), ph::make (30, 9, 4)), N >= 2);
  ASSERT_EQ (maybe_le (ch::make (31), ph::make (30, 0, 4)), N == 3);
  ASSERT_FALSE (maybe_le (ch::make (31), ph::make (30, 0, 0)));

  /* Test maybe_le (T, T).  */
  ASSERT_EQ (maybe_le (ph::make (3, 14, 99), ph::make (2, 15, 100)), N >= 2);
  ASSERT_EQ (maybe_le (ph::make (3, 14, 99), ph::make (2, 13, 100)), N == 3);
  ASSERT_EQ (maybe_le (ph::make (3, 14, 99), ph::make (2, 15, 98)), N >= 2);
  ASSERT_FALSE (maybe_le (ph::make (3, 14, 99), ph::make (2, 14, 99)));
  ASSERT_FALSE (maybe_le (ph::make (3, 14, 99), ph::make (2, 13, 98)));
  ASSERT_TRUE (maybe_le (ph::make (2, 14, 99), ph::make (2, 15, 100)));
  ASSERT_TRUE (maybe_le (ph::make (2, 14, 99), ph::make (2, 14, 99)));
  ASSERT_TRUE (maybe_le (ph::make (2, 14, 99), ph::make (2, 13, 98)));
  ASSERT_TRUE (maybe_le (ph::make (1, 14, 99), ph::make (2, 15, 100)));
  ASSERT_TRUE (maybe_le (ph::make (1, 14, 99), ph::make (2, 14, 99)));
  ASSERT_TRUE (maybe_le (ph::make (1, 14, 99), ph::make (2, 13, 98)));
}

/* Test maybe_lt for both signed and unsigned C.  */

template<unsigned int N, typename C, typename T>
static void
test_maybe_lt ()
{
  typedef coeff_helper<C> ch;
  typedef poly_helper<T> ph;

  /* Test maybe_lt (T, C).  */
  ASSERT_FALSE (maybe_lt (ph::make (7, 5, 4), ch::make (6)));
  ASSERT_FALSE (maybe_lt (ph::make (7, 0, 0), ch::make (6)));
  ASSERT_FALSE (maybe_lt (ph::make (60, 1, 2), ch::make (60)));
  ASSERT_FALSE (maybe_lt (ph::make (60, 0, 0), ch::make (60)));
  ASSERT_TRUE (maybe_lt (ph::make (30, 9, 4), ch::make (31)));
  ASSERT_TRUE (maybe_lt (ph::make (30, 0, 0), ch::make (31)));

  /* Test maybe_lt (C, T).  */
  ASSERT_TRUE (maybe_lt (ch::make (6), ph::make (7, 5, 4)));
  ASSERT_TRUE (maybe_lt (ch::make (6), ph::make (7, 0, 0)));
  ASSERT_EQ (maybe_lt (ch::make (60), ph::make (60, 1, 2)), N >= 2);
  ASSERT_EQ (maybe_lt (ch::make (60), ph::make (60, 0, 2)), N == 3);
  ASSERT_FALSE (maybe_lt (ch::make (60), ph::make (60, 0, 0)));
  ASSERT_EQ (maybe_lt (ch::make (31), ph::make (30, 9, 4)), N >= 2);
  ASSERT_EQ (maybe_lt (ch::make (31), ph::make (30, 0, 4)), N == 3);
  ASSERT_FALSE (maybe_lt (ch::make (31), ph::make (30, 0, 0)));

  /* Test maybe_lt (T, T).  */
  ASSERT_EQ (maybe_lt (ph::make (3, 14, 99), ph::make (2, 15, 100)), N >= 2);
  ASSERT_EQ (maybe_lt (ph::make (3, 14, 99), ph::make (2, 13, 100)), N == 3);
  ASSERT_EQ (maybe_lt (ph::make (3, 14, 99), ph::make (2, 15, 98)), N >= 2);
  ASSERT_FALSE (maybe_lt (ph::make (3, 14, 99), ph::make (2, 14, 99)));
  ASSERT_FALSE (maybe_lt (ph::make (3, 14, 99), ph::make (2, 13, 98)));
  ASSERT_EQ (maybe_lt (ph::make (2, 14, 99), ph::make (2, 15, 100)), N >= 2);
  ASSERT_EQ (maybe_lt (ph::make (2, 14, 99), ph::make (2, 13, 100)), N == 3);
  ASSERT_EQ (maybe_lt (ph::make (2, 14, 99), ph::make (2, 15, 98)), N >= 2);
  ASSERT_FALSE (maybe_lt (ph::make (2, 14, 99), ph::make (2, 14, 99)));
  ASSERT_FALSE (maybe_lt (ph::make (2, 14, 99), ph::make (2, 13, 98)));
  ASSERT_TRUE (maybe_lt (ph::make (1, 14, 99), ph::make (2, 15, 100)));
  ASSERT_TRUE (maybe_lt (ph::make (1, 14, 99), ph::make (2, 14, 99)));
  ASSERT_TRUE (maybe_lt (ph::make (1, 14, 99), ph::make (2, 13, 98)));
}

/* Test maybe_ge for both signed and unsigned C.  */

template<unsigned int N, typename C, typename T>
static void
test_maybe_ge ()
{
  typedef coeff_helper<C> ch;
  typedef poly_helper<T> ph;

  /* Test maybe_ge (T, C).  */
  ASSERT_TRUE (maybe_ge (ph::make (7, 5, 4), ch::make (6)));
  ASSERT_TRUE (maybe_ge (ph::make (7, 0, 0), ch::make (6)));
  ASSERT_TRUE (maybe_ge (ph::make (60, 1, 2), ch::make (60)));
  ASSERT_TRUE (maybe_ge (ph::make (60, 0, 0), ch::make (60)));
  ASSERT_EQ (maybe_ge (ph::make (30, 9, 4), ch::make (31)), N >= 2);
  ASSERT_EQ (maybe_ge (ph::make (30, 0, 4), ch::make (31)), N == 3);
  ASSERT_FALSE (maybe_ge (ph::make (30, 0, 0), ch::make (31)));

  /* Test maybe_ge (C, T).  */
  ASSERT_FALSE (maybe_ge (ch::make (6), ph::make (7, 5, 4)));
  ASSERT_FALSE (maybe_ge (ch::make (6), ph::make (7, 0, 0)));
  ASSERT_TRUE (maybe_ge (ch::make (60), ph::make (60, 1, 2)));
  ASSERT_TRUE (maybe_ge (ch::make (60), ph::make (60, 0, 0)));
  ASSERT_TRUE (maybe_ge (ch::make (31), ph::make (30, 9, 4)));
  ASSERT_TRUE (maybe_ge (ch::make (31), ph::make (30, 0, 0)));

  /* Test maybe_ge (T, T).  */
  ASSERT_TRUE (maybe_ge (ph::make (3, 14, 99), ph::make (2, 15, 100)));
  ASSERT_TRUE (maybe_ge (ph::make (3, 14, 99), ph::make (2, 14, 99)));
  ASSERT_TRUE (maybe_ge (ph::make (3, 14, 99), ph::make (2, 13, 98)));
  ASSERT_TRUE (maybe_ge (ph::make (2, 14, 99), ph::make (2, 15, 100)));
  ASSERT_TRUE (maybe_ge (ph::make (2, 14, 99), ph::make (2, 14, 99)));
  ASSERT_TRUE (maybe_ge (ph::make (2, 14, 99), ph::make (2, 13, 98)));
  ASSERT_FALSE (maybe_ge (ph::make (1, 14, 99), ph::make (2, 15, 100)));
  ASSERT_FALSE (maybe_ge (ph::make (1, 14, 99), ph::make (2, 14, 99)));
  ASSERT_EQ (maybe_ge (ph::make (1, 14, 99), ph::make (2, 15, 98)), N == 3);
  ASSERT_EQ (maybe_ge (ph::make (1, 14, 99), ph::make (2, 13, 100)), N >= 2);
  ASSERT_EQ (maybe_ge (ph::make (1, 14, 99), ph::make (2, 13, 98)), N >= 2);
}

/* Test maybe_gt for both signed and unsigned C.  */

template<unsigned int N, typename C, typename T>
static void
test_maybe_gt ()
{
  typedef coeff_helper<C> ch;
  typedef poly_helper<T> ph;

  /* Test maybe_gt (T, C).  */
  ASSERT_TRUE (maybe_gt (ph::make (7, 5, 4), ch::make (6)));
  ASSERT_TRUE (maybe_gt (ph::make (7, 0, 0), ch::make (6)));
  ASSERT_EQ (maybe_gt (ph::make (60, 1, 2), ch::make (60)), N >= 2);
  ASSERT_EQ (maybe_gt (ph::make (60, 0, 2), ch::make (60)), N == 3);
  ASSERT_FALSE (maybe_gt (ph::make (60, 0, 0), ch::make (60)));
  ASSERT_EQ (maybe_gt (ph::make (30, 9, 4), ch::make (31)), N >= 2);
  ASSERT_EQ (maybe_gt (ph::make (30, 0, 4), ch::make (31)), N == 3);
  ASSERT_FALSE (maybe_gt (ph::make (30, 0, 0), ch::make (31)));

  /* Test maybe_gt (C, T).  */
  ASSERT_FALSE (maybe_gt (ch::make (6), ph::make (7, 5, 4)));
  ASSERT_FALSE (maybe_gt (ch::make (6), ph::make (7, 0, 0)));
  ASSERT_FALSE (maybe_gt (ch::make (60), ph::make (60, 1, 2)));
  ASSERT_FALSE (maybe_gt (ch::make (60), ph::make (60, 0, 0)));
  ASSERT_TRUE (maybe_gt (ch::make (31), ph::make (30, 9, 4)));
  ASSERT_TRUE (maybe_gt (ch::make (31), ph::make (30, 0, 0)));

  /* Test maybe_gt (T, T).  */
  ASSERT_TRUE (maybe_gt (ph::make (3, 14, 99), ph::make (2, 15, 100)));
  ASSERT_TRUE (maybe_gt (ph::make (3, 14, 99), ph::make (2, 14, 99)));
  ASSERT_TRUE (maybe_gt (ph::make (3, 14, 99), ph::make (2, 13, 98)));
  ASSERT_FALSE (maybe_gt (ph::make (2, 14, 99), ph::make (2, 15, 100)));
  ASSERT_FALSE (maybe_gt (ph::make (2, 14, 99), ph::make (2, 14, 99)));
  ASSERT_EQ (maybe_gt (ph::make (2, 14, 99), ph::make (2, 15, 98)), N == 3);
  ASSERT_EQ (maybe_gt (ph::make (2, 14, 99), ph::make (2, 13, 100)), N >= 2);
  ASSERT_EQ (maybe_gt (ph::make (2, 14, 99), ph::make (2, 13, 98)), N >= 2);
  ASSERT_FALSE (maybe_gt (ph::make (1, 14, 99), ph::make (2, 15, 100)));
  ASSERT_FALSE (maybe_gt (ph::make (1, 14, 99), ph::make (2, 14, 99)));
  ASSERT_EQ (maybe_gt (ph::make (1, 14, 99), ph::make (2, 15, 98)), N == 3);
  ASSERT_EQ (maybe_gt (ph::make (1, 14, 99), ph::make (2, 13, 100)), N >= 2);
  ASSERT_EQ (maybe_gt (ph::make (1, 14, 99), ph::make (2, 13, 98)), N >= 2);
}

/* Test known_gt for both signed and unsigned C.  */

template<unsigned int N, typename C, typename T>
static void
test_known_gt ()
{
  typedef coeff_helper<C> ch;
  typedef poly_helper<T> ph;

  /* Test known_gt (T, C).  */
  ASSERT_TRUE (known_gt (ph::make (7, 5, 4), ch::make (6)));
  ASSERT_TRUE (known_gt (ph::make (7, 0, 0), ch::make (6)));
  ASSERT_FALSE (known_gt (ph::make (60, 1, 2), ch::make (60)));
  ASSERT_FALSE (known_gt (ph::make (60, 0, 0), ch::make (60)));
  ASSERT_FALSE (known_gt (ph::make (30, 9, 4), ch::make (31)));
  ASSERT_FALSE (known_gt (ph::make (30, 0, 0), ch::make (31)));

  /* Test known_gt (C, T).  */
  ASSERT_FALSE (known_gt (ch::make (6), ph::make (7, 5, 4)));
  ASSERT_FALSE (known_gt (ch::make (6), ph::make (7, 0, 0)));
  ASSERT_FALSE (known_gt (ch::make (60), ph::make (60, 1, 2)));
  ASSERT_FALSE (known_gt (ch::make (60), ph::make (60, 0, 0)));
  ASSERT_EQ (known_gt (ch::make (31), ph::make (30, 9, 4)), N == 1);
  ASSERT_EQ (known_gt (ch::make (31), ph::make (30, 0, 4)), N <= 2);
  ASSERT_TRUE (known_gt (ch::make (31), ph::make (30, 0, 0)));

  /* Test known_gt (T, T).  */
  ASSERT_EQ (known_gt (ph::make (3, 14, 99), ph::make (2, 15, 100)), N == 1);
  ASSERT_EQ (known_gt (ph::make (3, 14, 99), ph::make (2, 13, 100)), N <= 2);
  ASSERT_EQ (known_gt (ph::make (3, 14, 99), ph::make (2, 15, 98)), N == 1);
  ASSERT_TRUE (known_gt (ph::make (3, 14, 99), ph::make (2, 14, 99)));
  ASSERT_TRUE (known_gt (ph::make (3, 14, 99), ph::make (2, 13, 98)));
  ASSERT_FALSE (known_gt (ph::make (2, 14, 99), ph::make (2, 15, 100)));
  ASSERT_FALSE (known_gt (ph::make (2, 14, 99), ph::make (2, 14, 99)));
  ASSERT_FALSE (known_gt (ph::make (2, 14, 99), ph::make (2, 13, 98)));
  ASSERT_FALSE (known_gt (ph::make (1, 14, 99), ph::make (2, 15, 100)));
  ASSERT_FALSE (known_gt (ph::make (1, 14, 99), ph::make (2, 14, 99)));
  ASSERT_FALSE (known_gt (ph::make (1, 14, 99), ph::make (2, 13, 98)));
}

/* Test known_ge for both signed and unsigned C.  */

template<unsigned int N, typename C, typename T>
static void
test_known_ge ()
{
  typedef coeff_helper<C> ch;
  typedef poly_helper<T> ph;

  /* Test known_ge (T, C).  */
  ASSERT_TRUE (known_ge (ph::make (7, 5, 4), ch::make (6)));
  ASSERT_TRUE (known_ge (ph::make (7, 0, 0), ch::make (6)));
  ASSERT_TRUE (known_ge (ph::make (60, 1, 2), ch::make (60)));
  ASSERT_TRUE (known_ge (ph::make (60, 0, 0), ch::make (60)));
  ASSERT_FALSE (known_ge (ph::make (30, 9, 4), ch::make (31)));
  ASSERT_FALSE (known_ge (ph::make (30, 0, 0), ch::make (31)));

  /* Test known_ge (C, T).  */
  ASSERT_FALSE (known_ge (ch::make (6), ph::make (7, 5, 4)));
  ASSERT_FALSE (known_ge (ch::make (6), ph::make (7, 0, 0)));
  ASSERT_EQ (known_ge (ch::make (60), ph::make (60, 1, 2)), N == 1);
  ASSERT_EQ (known_ge (ch::make (60), ph::make (60, 0, 2)), N <= 2);
  ASSERT_TRUE (known_ge (ch::make (60), ph::make (60, 0, 0)));
  ASSERT_EQ (known_ge (ch::make (31), ph::make (30, 9, 4)), N == 1);
  ASSERT_EQ (known_ge (ch::make (31), ph::make (30, 0, 4)), N <= 2);
  ASSERT_TRUE (known_ge (ch::make (31), ph::make (30, 0, 0)));

  /* Test known_ge (T, T).  */
  ASSERT_EQ (known_ge (ph::make (3, 14, 99), ph::make (2, 15, 100)), N == 1);
  ASSERT_EQ (known_ge (ph::make (3, 14, 99), ph::make (2, 13, 100)), N <= 2);
  ASSERT_EQ (known_ge (ph::make (3, 14, 99), ph::make (2, 15, 98)), N == 1);
  ASSERT_TRUE (known_ge (ph::make (3, 14, 99), ph::make (2, 14, 99)));
  ASSERT_TRUE (known_ge (ph::make (3, 14, 99), ph::make (2, 13, 98)));
  ASSERT_EQ (known_ge (ph::make (2, 14, 99), ph::make (2, 15, 100)), N == 1);
  ASSERT_EQ (known_ge (ph::make (2, 14, 99), ph::make (2, 13, 100)), N <= 2);
  ASSERT_EQ (known_ge (ph::make (2, 14, 99), ph::make (2, 15, 98)), N == 1);
  ASSERT_TRUE (known_ge (ph::make (2, 14, 99), ph::make (2, 14, 99)));
  ASSERT_TRUE (known_ge (ph::make (2, 14, 99), ph::make (2, 13, 98)));
  ASSERT_FALSE (known_ge (ph::make (1, 14, 99), ph::make (2, 15, 100)));
  ASSERT_FALSE (known_ge (ph::make (1, 14, 99), ph::make (2, 14, 99)));
  ASSERT_FALSE (known_ge (ph::make (1, 14, 99), ph::make (2, 13, 98)));
}

/* Test known_lt for both signed and unsigned C.  */

template<unsigned int N, typename C, typename T>
static void
test_known_lt ()
{
  typedef coeff_helper<C> ch;
  typedef poly_helper<T> ph;

  /* Test known_lt (T, C).  */
  ASSERT_FALSE (known_lt (ph::make (7, 5, 4), ch::make (6)));
  ASSERT_FALSE (known_lt (ph::make (7, 0, 0), ch::make (6)));
  ASSERT_FALSE (known_lt (ph::make (60, 1, 2), ch::make (60)));
  ASSERT_FALSE (known_lt (ph::make (60, 0, 0), ch::make (60)));
  ASSERT_EQ (known_lt (ph::make (30, 9, 4), ch::make (31)), N == 1);
  ASSERT_EQ (known_lt (ph::make (30, 0, 4), ch::make (31)), N <= 2);
  ASSERT_TRUE (known_lt (ph::make (30, 0, 0), ch::make (31)));

  /* Test known_lt (C, T).  */
  ASSERT_TRUE (known_lt (ch::make (6), ph::make (7, 5, 4)));
  ASSERT_TRUE (known_lt (ch::make (6), ph::make (7, 0, 0)));
  ASSERT_FALSE (known_lt (ch::make (60), ph::make (60, 1, 2)));
  ASSERT_FALSE (known_lt (ch::make (60), ph::make (60, 0, 0)));
  ASSERT_FALSE (known_lt (ch::make (31), ph::make (30, 9, 4)));
  ASSERT_FALSE (known_lt (ch::make (31), ph::make (30, 0, 0)));

  /* Test known_lt (T, T).  */
  ASSERT_FALSE (known_lt (ph::make (3, 14, 99), ph::make (2, 15, 100)));
  ASSERT_FALSE (known_lt (ph::make (3, 14, 99), ph::make (2, 14, 99)));
  ASSERT_FALSE (known_lt (ph::make (3, 14, 99), ph::make (2, 13, 98)));
  ASSERT_FALSE (known_lt (ph::make (2, 14, 99), ph::make (2, 15, 100)));
  ASSERT_FALSE (known_lt (ph::make (2, 14, 99), ph::make (2, 14, 99)));
  ASSERT_FALSE (known_lt (ph::make (2, 14, 99), ph::make (2, 13, 98)));
  ASSERT_TRUE (known_lt (ph::make (1, 14, 99), ph::make (2, 15, 100)));
  ASSERT_TRUE (known_lt (ph::make (1, 14, 99), ph::make (2, 14, 99)));
  ASSERT_EQ (known_lt (ph::make (1, 14, 99), ph::make (2, 15, 98)), N <= 2);
  ASSERT_EQ (known_lt (ph::make (1, 14, 99), ph::make (2, 13, 100)), N == 1);
  ASSERT_EQ (known_lt (ph::make (1, 14, 99), ph::make (2, 13, 98)), N == 1);
}

/* Test known_le for both signed and unsigned C.  */

template<unsigned int N, typename C, typename T>
static void
test_known_le ()
{
  typedef coeff_helper<C> ch;
  typedef poly_helper<T> ph;

  /* Test known_le (T, C).  */
  ASSERT_FALSE (known_le (ph::make (7, 5, 4), ch::make (6)));
  ASSERT_FALSE (known_le (ph::make (7, 0, 0), ch::make (6)));
  ASSERT_EQ (known_le (ph::make (60, 1, 2), ch::make (60)), N == 1);
  ASSERT_EQ (known_le (ph::make (60, 0, 2), ch::make (60)), N <= 2);
  ASSERT_TRUE (known_le (ph::make (60, 0, 0), ch::make (60)));
  ASSERT_EQ (known_le (ph::make (30, 9, 4), ch::make (31)), N == 1);
  ASSERT_EQ (known_le (ph::make (30, 0, 4), ch::make (31)), N <= 2);
  ASSERT_TRUE (known_le (ph::make (30, 0, 0), ch::make (31)));

  /* Test known_le (C, T).  */
  ASSERT_TRUE (known_le (ch::make (6), ph::make (7, 5, 4)));
  ASSERT_TRUE (known_le (ch::make (6), ph::make (7, 0, 0)));
  ASSERT_TRUE (known_le (ch::make (60), ph::make (60, 1, 2)));
  ASSERT_TRUE (known_le (ch::make (60), ph::make (60, 0, 0)));
  ASSERT_FALSE (known_le (ch::make (31), ph::make (30, 9, 4)));
  ASSERT_FALSE (known_le (ch::make (31), ph::make (30, 0, 0)));

  /* Test known_le (T, T).  */
  ASSERT_FALSE (known_le (ph::make (3, 14, 99), ph::make (2, 15, 100)));
  ASSERT_FALSE (known_le (ph::make (3, 14, 99), ph::make (2, 14, 99)));
  ASSERT_FALSE (known_le (ph::make (3, 14, 99), ph::make (2, 13, 98)));
  ASSERT_TRUE (known_le (ph::make (2, 14, 99), ph::make (2, 15, 100)));
  ASSERT_TRUE (known_le (ph::make (2, 14, 99), ph::make (2, 14, 99)));
  ASSERT_EQ (known_le (ph::make (2, 14, 99), ph::make (2, 15, 98)), N <= 2);
  ASSERT_EQ (known_le (ph::make (2, 14, 99), ph::make (2, 13, 100)), N == 1);
  ASSERT_EQ (known_le (ph::make (2, 14, 99), ph::make (2, 13, 98)), N == 1);
  ASSERT_TRUE (known_le (ph::make (1, 14, 99), ph::make (2, 15, 100)));
  ASSERT_TRUE (known_le (ph::make (1, 14, 99), ph::make (2, 14, 99)));
  ASSERT_EQ (known_le (ph::make (1, 14, 99), ph::make (2, 15, 98)), N <= 2);
  ASSERT_EQ (known_le (ph::make (1, 14, 99), ph::make (2, 13, 100)), N == 1);
  ASSERT_EQ (known_le (ph::make (1, 14, 99), ph::make (2, 13, 98)), N == 1);
}

/* Test ordered_p for both signed and unsigned C.  */

template<unsigned int N, typename C, typename T>
static void
test_ordered_p ()
{
  typedef coeff_helper<C> ch;
  typedef poly_helper<T> ph;

  /* Test ordered_p (T, C).  */
  ASSERT_EQ (ordered_p (ph::make (4, 1, 2), ch::make (5)), N == 1);
  ASSERT_EQ (ordered_p (ph::make (4, 0, 2), ch::make (5)), N <= 2);
  ASSERT_TRUE (ordered_p (ph::make (4, 0, 0), ch::make (5)));
  ASSERT_TRUE (ordered_p (ph::make (4, 1, 2), ch::make (4)));
  ASSERT_TRUE (ordered_p (ph::make (4, 0, 0), ch::make (4)));
  ASSERT_TRUE (ordered_p (ph::make (4, 1, 2), ch::make (3)));
  ASSERT_TRUE (ordered_p (ph::make (4, 0, 0), ch::make (3)));
  ASSERT_TRUE (ordered_p (ph::make (4, 4, 4), ch::make (0)));
  ASSERT_TRUE (ordered_p (ph::make (4, 4, 0), ch::make (0)));
  ASSERT_TRUE (ordered_p (ph::make (4, 0, 4), ch::make (0)));
  ASSERT_TRUE (ordered_p (ph::make (-4, -4, -4), ch::make (0)));
  ASSERT_TRUE (ordered_p (ph::make (-4, -4, 0), ch::make (0)));
  ASSERT_TRUE (ordered_p (ph::make (-4, 0, -4), ch::make (0)));

  /* Test ordered_p (C, T).  */
  ASSERT_EQ (ordered_p (ch::make (5), ph::make (4, 1, 2)), N == 1);
  ASSERT_EQ (ordered_p (ch::make (5), ph::make (4, 0, 2)), N <= 2);
  ASSERT_TRUE (ordered_p (ch::make (5), ph::make (4, 0, 0)));
  ASSERT_TRUE (ordered_p (ch::make (4), ph::make (4, 1, 2)));
  ASSERT_TRUE (ordered_p (ch::make (4), ph::make (4, 0, 0)));
  ASSERT_TRUE (ordered_p (ch::make (3), ph::make (4, 1, 2)));
  ASSERT_TRUE (ordered_p (ch::make (3), ph::make (4, 0, 0)));
  ASSERT_TRUE (ordered_p (ch::make (0), ph::make (4, 4, 4)));
  ASSERT_TRUE (ordered_p (ch::make (0), ph::make (4, 4, 0)));
  ASSERT_TRUE (ordered_p (ch::make (0), ph::make (4, 0, 4)));
  ASSERT_TRUE (ordered_p (ch::make (0), ph::make (-4, -4, -4)));
  ASSERT_TRUE (ordered_p (ch::make (0), ph::make (-4, -4, 0)));
  ASSERT_TRUE (ordered_p (ch::make (0), ph::make (-4, 0, -4)));

  /* Test ordered_p (T, T).  */
  ASSERT_EQ (ordered_p (ph::make (3, 14, 99), ph::make (2, 15, 100)), N == 1);
  ASSERT_EQ (ordered_p (ph::make (3, 14, 99), ph::make (2, 13, 100)), N <= 2);
  ASSERT_EQ (ordered_p (ph::make (3, 14, 99), ph::make (2, 15, 98)), N == 1);
  ASSERT_TRUE (ordered_p (ph::make (3, 14, 99), ph::make (2, 14, 99)));
  ASSERT_TRUE (ordered_p (ph::make (3, 14, 99), ph::make (2, 13, 98)));
  ASSERT_TRUE (ordered_p (ph::make (2, 14, 99), ph::make (2, 15, 100)));
  ASSERT_TRUE (ordered_p (ph::make (2, 14, 99), ph::make (2, 14, 100)));
  ASSERT_TRUE (ordered_p (ph::make (2, 14, 99), ph::make (2, 15, 99)));
  ASSERT_EQ (ordered_p (ph::make (2, 14, 99), ph::make (2, 13, 100)), N <= 2);
  ASSERT_TRUE (ordered_p (ph::make (2, 14, 99), ph::make (2, 14, 99)));
  ASSERT_EQ (ordered_p (ph::make (2, 14, 99), ph::make (2, 15, 98)), N <= 2);
  ASSERT_TRUE (ordered_p (ph::make (2, 14, 99), ph::make (2, 13, 99)));
  ASSERT_TRUE (ordered_p (ph::make (2, 14, 99), ph::make (2, 14, 98)));
  ASSERT_TRUE (ordered_p (ph::make (2, 14, 99), ph::make (2, 13, 98)));
  ASSERT_TRUE (ordered_p (ph::make (1, 14, 99), ph::make (2, 15, 100)));
  ASSERT_TRUE (ordered_p (ph::make (1, 14, 99), ph::make (2, 14, 99)));
  ASSERT_EQ (ordered_p (ph::make (1, 14, 99), ph::make (2, 15, 98)), N <= 2);
  ASSERT_EQ (ordered_p (ph::make (1, 14, 99), ph::make (2, 13, 100)), N == 1);
  ASSERT_EQ (ordered_p (ph::make (1, 14, 99), ph::make (2, 13, 98)), N == 1);
}

/* Test ordered_min for both signed and unsigned C.  */

template<unsigned int N, typename C, typename T>
static void
test_ordered_min ()
{
  typedef coeff_helper<C> ch;
  typedef poly_helper<T> ph;

  /* Test ordered_min (T, C).  */
  ASSERT_KNOWN_EQ (ordered_min (ph::make (4, 0, 0), ch::make (5)),
		   ch::make (4));
  ASSERT_KNOWN_EQ (ordered_min (ph::make (12, 0, 0), ch::make (11)),
		   ch::make (11));
  ASSERT_KNOWN_EQ (ordered_min (ph::make (12, 6, 4), ch::make (11)),
		   ch::make (11));

  /* Test ordered_min (C, T).  */
  ASSERT_KNOWN_EQ (ordered_min (ch::make (5), ph::make (4, 0, 0)),
		   ch::make (4));
  ASSERT_KNOWN_EQ (ordered_min (ch::make (11), ph::make (12, 0, 0)),
		   ch::make (11));
  ASSERT_KNOWN_EQ (ordered_min (ch::make (11), ph::make (12, 6, 4)),
		   ch::make (11));

  /* Test ordered_min (T, T).  */
  ASSERT_KNOWN_EQ (ordered_min (ph::make (4, 6, 14), ph::make (5, 6, 19)),
		   ph::make (4, 6, 14));
  ASSERT_KNOWN_EQ (ordered_min (ph::make (4, 9, 17), ph::make (3, 9, 0)),
		   ph::make (3, 9, 0));
  ASSERT_KNOWN_EQ (ordered_min (ph::make (-4, -5, 12), ph::make (-3, -5, 12)),
		   ph::make (-4, -5, 12));
  ASSERT_KNOWN_EQ (ordered_min (ph::make (4, -9, 6), ph::make (4, -8, 6)),
		   ph::make (4, -9, 6));
  ASSERT_KNOWN_EQ (ordered_min (ph::make (5, -1, -14), ph::make (5, -1, -16)),
		   ph::make (5, -1, -16));
}

/* Test ordered_max for both signed and unsigned C.  */

template<unsigned int N, typename C, typename T>
static void
test_ordered_max ()
{
  typedef coeff_helper<C> ch;
  typedef poly_helper<T> ph;

  /* Test ordered_max (T, C).  */
  ASSERT_KNOWN_EQ (ordered_max (ph::make (4, 0, 0), ch::make (5)),
		   ch::make (5));
  ASSERT_KNOWN_EQ (ordered_max (ph::make (12, 0, 0), ch::make (11)),
		   ch::make (12));
  ASSERT_KNOWN_EQ (ordered_max (ph::make (12, 6, 4), ch::make (11)),
		   ph::make (12, 6, 4));

  /* Test ordered_max (C, T).  */
  ASSERT_KNOWN_EQ (ordered_max (ch::make (5), ph::make (4, 0, 0)),
		   ch::make (5));
  ASSERT_KNOWN_EQ (ordered_max (ch::make (11), ph::make (12, 0, 0)),
		   ch::make (12));
  ASSERT_KNOWN_EQ (ordered_max (ch::make (11), ph::make (12, 6, 4)),
		   ph::make (12, 6, 4));

  /* Test ordered_max (T, T).  */
  ASSERT_KNOWN_EQ (ordered_max (ph::make (4, 6, 14), ph::make (5, 6, 19)),
		   ph::make (5, 6, 19));
  ASSERT_KNOWN_EQ (ordered_max (ph::make (4, 9, 17), ph::make (3, 9, 0)),
		   ph::make (4, 9, 17));
  ASSERT_KNOWN_EQ (ordered_max (ph::make (-4, -5, 12), ph::make (-3, -5, 12)),
		   ph::make (-3, -5, 12));
  ASSERT_KNOWN_EQ (ordered_max (ph::make (4, -9, 6), ph::make (4, -8, 6)),
		   ph::make (4, -8, 6));
  ASSERT_KNOWN_EQ (ordered_max (ph::make (5, -1, -14), ph::make (5, -1, -16)),
		   ph::make (5, -1, -14));
}

/* Test constant_lower_bound for both signed and unsigned C.  */

template<unsigned int N, typename C, typename T>
static void
test_constant_lower_bound ()
{
  typedef poly_helper<T> ph;

  ASSERT_EQ (constant_lower_bound (ph::make (4, 1, 2)), 4);
  ASSERT_EQ (constant_lower_bound (ph::make (5, 0, 1)), 5);
  ASSERT_EQ (constant_lower_bound (ph::make (6, 1, 0)), 6);
  ASSERT_EQ (constant_lower_bound (ph::make (7, 0, 0)), 7);
}

/* Test lower_bound for both signed and unsigned C.  */

template<unsigned int N, typename C, typename T>
static void
test_lower_bound ()
{
  typedef coeff_helper<C> ch;
  typedef poly_helper<T> ph;

  /* Test lower_bound (T, C).  */
  ASSERT_KNOWN_EQ (lower_bound (ph::make (7, 2, 15), ch::make (4)),
		   ch::make (4));
  ASSERT_KNOWN_EQ (lower_bound (ph::make (100, 5, 50), ch::make (200)),
		   ch::make (100));

  /* Test lower_bound (C, T).  */
  ASSERT_KNOWN_EQ (lower_bound (ch::make (4), ph::make (7, 2, 15)),
		   ch::make (4));
  ASSERT_KNOWN_EQ (lower_bound (ch::make (200), ph::make (100, 5, 50)),
		   ch::make (100));

  /* Test lower_bound (T, T).  */
  ASSERT_KNOWN_EQ (lower_bound (ph::make (7, 2, 15), ph::make (5, 19, 14)),
		   ph::make (5, 2, 14));
  ASSERT_KNOWN_EQ (lower_bound (ph::make (100, 5, 50), ph::make (200, 0, 80)),
		   ph::make (100, 0, 50));
}

/* Test upper_bound for both signed and unsigned C.  */

template<unsigned int N, typename C, typename T>
static void
test_upper_bound ()
{
  typedef coeff_helper<C> ch;
  typedef poly_helper<T> ph;

  /* Test upper_bound (T, C).  */
  ASSERT_KNOWN_EQ (upper_bound (ph::make (7, 2, 15), ch::make (4)),
		   ph::make (7, 2, 15));
  ASSERT_KNOWN_EQ (upper_bound (ph::make (100, 5, 50), ch::make (200)),
		   ph::make (200, 5, 50));

  /* Test upper_bound (C, T).  */
  ASSERT_KNOWN_EQ (upper_bound (ch::make (4), ph::make (7, 2, 15)),
		   ph::make (7, 2, 15));
  ASSERT_KNOWN_EQ (upper_bound (ch::make (200), ph::make (100, 5, 50)),
		   ph::make (200, 5, 50));

  /* Test upper_bound (T, T).  */
  ASSERT_KNOWN_EQ (upper_bound (ph::make (7, 2, 15), ph::make (5, 19, 14)),
		   ph::make (7, 19, 15));
  ASSERT_KNOWN_EQ (upper_bound (ph::make (100, 5, 50), ph::make (200, 0, 80)),
		   ph::make (200, 5, 80));
}

/* Test compare_sizes_for_sort for both signed and unsigned C.  */

template<unsigned int N, typename C, typename T>
static void
test_compare_sizes_for_sort ()
{
  typedef poly_helper<T> ph;

  ASSERT_EQ (compare_sizes_for_sort (ph::make (5, 10, 8),
				     ph::make (7, 9, 11)),
	     N == 2 ? 1 : -1);
  ASSERT_EQ (compare_sizes_for_sort (ph::make (5, 9, 8),
				     ph::make (7, 9, 11)),
	     -1);
  ASSERT_EQ (compare_sizes_for_sort (ph::make (19, 9, 13),
				     ph::make (7, 9, 13)),
	     1);
  ASSERT_EQ (compare_sizes_for_sort (ph::make (5, 9, 7),
				     ph::make (5, 10, 5)),
	     N == 1 ? 0 : N == 2 ? -1 : 1);
  ASSERT_EQ (compare_sizes_for_sort (ph::make (10, 9, 10),
				     ph::make (10, 9, 6)),
	     N <= 2 ? 0 : 1);
  ASSERT_EQ (compare_sizes_for_sort (ph::make (10, 9, 6),
				     ph::make (10, 9, 6)),
	     0);
}

/* Test force_align_up_and_div for both signed and unsigned C.  */

template<unsigned int N, typename C, typename T>
static void
test_force_align_up_and_div ()
{
  typedef coeff_helper<C> ch;
  typedef poly_helper<T> ph;

  ASSERT_KNOWN_EQ (force_align_up_and_div (ph::make (41, 32, 16), 16),
		   ph::make (3, 2, 1));
  ASSERT_KNOWN_EQ (force_align_up_and_div (ph::make (-39, -64, -32), 32),
		   ph::make (C (-32) / 32, C (-64) / 32, C (-32) / 32));
  ASSERT_KNOWN_EQ (force_align_up_and_div (ph::make (17, 0, 0), 16),
		   ch::make (2));
  ASSERT_KNOWN_EQ (force_align_up_and_div (ph::make (16, 0, 0), 16),
		   ch::make (1));
  ASSERT_KNOWN_EQ (force_align_up_and_div (ph::make (15, 0, 0), 16),
		   ch::make (1));
  ASSERT_KNOWN_EQ (force_align_up_and_div (ph::make (-17, 0, 0), 16),
		   ch::make (C (-16) / 16));
  ASSERT_KNOWN_EQ (force_align_up_and_div (ph::make (-16, 0, 0), 16),
		   ch::make (C (-16) / 16));
  /* For unsigned short C this gives 0x10000 / 16.  */
  ASSERT_KNOWN_EQ (force_align_up_and_div (ph::make (-15, 0, 0), 16),
		   ch::make ((C (-1) + 1) / 16));
}

/* Test force_align_down_and_div for both signed and unsigned C.  */

template<unsigned int N, typename C, typename T>
static void
test_force_align_down_and_div ()
{
  typedef coeff_helper<C> ch;
  typedef poly_helper<T> ph;

  ASSERT_KNOWN_EQ (force_align_down_and_div (ph::make (41, 32, 16), 16),
		   ph::make (2, 2, 1));
  ASSERT_KNOWN_EQ (force_align_down_and_div (ph::make (-39, -64, -32), 32),
		   ph::make (C (-64) / 32, C (-64) / 32, C (-32) / 32));
  ASSERT_KNOWN_EQ (force_align_down_and_div (ph::make (17, 0, 0), 16),
		   ch::make (1));
  ASSERT_KNOWN_EQ (force_align_down_and_div (ph::make (16, 0, 0), 16),
		   ch::make (1));
  ASSERT_KNOWN_EQ (force_align_down_and_div (ph::make (15, 0, 0), 16),
		   ch::make (0));
  ASSERT_KNOWN_EQ (force_align_down_and_div (ph::make (-17, 0, 0), 16),
		   ch::make (C (-32) / 16));
  ASSERT_KNOWN_EQ (force_align_down_and_div (ph::make (-16, 0, 0), 16),
		   ch::make (C (-16) / 16));
  ASSERT_KNOWN_EQ (force_align_down_and_div (ph::make (-15, 0, 0), 16),
		   ch::make (C (-16) / 16));
}

/* Test constant_multiple_p for both signed and unsigned C.  */

template<unsigned int N, typename C, typename T>
static void
test_constant_multiple_p ()
{
  typedef poly_helper<T> ph;

  /* Test constant_multiple_p (T, C).  */
  C const_multiple;
  ASSERT_TRUE (constant_multiple_p (ph::make (15, 0, 0), 5,
				    &const_multiple));
  ASSERT_EQ (const_multiple, 3);
  ASSERT_FALSE (constant_multiple_p (ph::make (16, 0, 0), 5,
				     &const_multiple));
  ASSERT_FALSE (constant_multiple_p (ph::make (14, 5, 5), 5,
				     &const_multiple));
  ASSERT_EQ (constant_multiple_p (ph::make (44, 0, 55), 11,
				  &const_multiple), N <= 2);
  ASSERT_EQ (const_multiple, N <= 2 ? 4 : 3);
  ASSERT_EQ (constant_multiple_p (ph::make (30, 30, 0), 6,
				  &const_multiple), N == 1);
  ASSERT_EQ (const_multiple, N == 1 ? 5 : N == 2 ? 4 : 3);
  ASSERT_TRUE (constant_multiple_p (ph::make (0, 0, 0), 5,
				    &const_multiple));

  /* Test constant_multiple_p (C, T).  */
  ASSERT_TRUE (constant_multiple_p (15, ph::make (5, 0, 0),
				    &const_multiple));
  ASSERT_EQ (const_multiple, 3);
  ASSERT_FALSE (constant_multiple_p (16, ph::make (5, 0, 0),
				     &const_multiple));
  ASSERT_FALSE (constant_multiple_p (14, ph::make (5, 5, 5),
				     &const_multiple));
  ASSERT_EQ (constant_multiple_p (44, ph::make (11, 0, 4),
				  &const_multiple), N <= 2);
  ASSERT_EQ (const_multiple, N <= 2 ? 4 : 3);
  ASSERT_EQ (constant_multiple_p (30, ph::make (6, 6, 6),
				  &const_multiple), N == 1);
  ASSERT_EQ (const_multiple, N == 1 ? 5 : N == 2 ? 4 : 3);
  ASSERT_TRUE (constant_multiple_p (0, ph::make (5, 4, 11),
				    &const_multiple));
  ASSERT_EQ (const_multiple, 0);

  /* Test constant_multiple_p (T, T).  */
  ASSERT_TRUE (constant_multiple_p (ph::make (5, 15, 25),
				    ph::make (1, 3, 5),
				    &const_multiple));
  ASSERT_EQ (const_multiple, 5);
  ASSERT_EQ (constant_multiple_p (ph::make (18, 30, 7),
				  ph::make (6, 10, 2),
				  &const_multiple), N <= 2);
  ASSERT_EQ (const_multiple, N <= 2 ? 3 : 5);
  ASSERT_EQ (constant_multiple_p (ph::make (54, 19, 0),
				  ph::make (9, 3, 0),
				  &const_multiple), N == 1);
  ASSERT_EQ (const_multiple, N == 1 ? 6 : N == 2 ? 3: 5);
  ASSERT_TRUE (constant_multiple_p (ph::make (120, 0, 90),
				    ph::make (12, 0, 9),
				    &const_multiple));
  ASSERT_EQ (const_multiple, 10);
  ASSERT_EQ (constant_multiple_p (ph::make (110, 1, 22),
				  ph::make (10, 0, 2),
				  &const_multiple), N == 1);
  ASSERT_EQ (const_multiple, N == 1 ? 11 : 10);
  ASSERT_EQ (constant_multiple_p (ph::make (120, -1, 22),
				  ph::make (10, 0, 2),
				  &const_multiple), N == 1);
  ASSERT_EQ (const_multiple, N == 1 ? 12 : 10);
  ASSERT_EQ (constant_multiple_p (ph::make (130, 0, 26),
				  ph::make (10, 1, 2),
				  &const_multiple), N == 1);
  ASSERT_EQ (const_multiple, N == 1 ? 13 : 10);
  ASSERT_EQ (constant_multiple_p (ph::make (140, 0, 28),
				  ph::make (10, -1, 2),
				  &const_multiple), N == 1);
  ASSERT_EQ (const_multiple, N == 1 ? 14 : 10);
  ASSERT_FALSE (constant_multiple_p (ph::make (89, 0, 0),
				     ph::make (11, 0, 0),
				     &const_multiple));
  ASSERT_TRUE (constant_multiple_p (ph::make (88, 0, 0),
				    ph::make (11, 0, 0),
				    &const_multiple));
  ASSERT_EQ (const_multiple, 8);
  ASSERT_FALSE (constant_multiple_p (ph::make (87, 0, 0),
				     ph::make (11, 0, 0),
				     &const_multiple));
  ASSERT_TRUE (constant_multiple_p (ph::make (35, 63, 0),
				    ph::make (5, 9, 0),
				    &const_multiple));
  ASSERT_EQ (const_multiple, 7);
  ASSERT_TRUE (constant_multiple_p (ph::make (0, 0, 0),
				    ph::make (11, -24, 25),
				    &const_multiple));
  ASSERT_EQ (const_multiple, 0);
}

/* Test multiple_p for both signed and unsigned C.  */

template<unsigned int N, typename C, typename T>
static void
test_multiple_p ()
{
  typedef poly_helper<T> ph;

  /* Test multiple_p (T, C).  */
  ASSERT_TRUE (multiple_p (ph::make (15, 0, 0), 5));
  ASSERT_FALSE (multiple_p (ph::make (16, 0, 0), 5));
  ASSERT_FALSE (multiple_p (ph::make (14, 5, 5), 5));
  ASSERT_TRUE (multiple_p (ph::make (44, 0, 55), 11));
  ASSERT_TRUE (multiple_p (ph::make (30, 30, 0), 6));
  ASSERT_TRUE (multiple_p (ph::make (30, 35, 45), 5));
  ASSERT_EQ (multiple_p (ph::make (30, 35, 44), 5), N <= 2);
  ASSERT_EQ (multiple_p (ph::make (30, 34, 45), 5), N == 1);
  ASSERT_TRUE (multiple_p (ph::make (0, 0, 0), 5));

  /* Test multiple_p (C, T).  */
  ASSERT_TRUE (multiple_p (15, ph::make (5, 0, 0)));
  ASSERT_FALSE (multiple_p (16, ph::make (5, 0, 0)));
  ASSERT_FALSE (multiple_p (14, ph::make (5, 5, 5)));
  ASSERT_EQ (multiple_p (44, ph::make (11, 0, 4)), N <= 2);
  ASSERT_EQ (multiple_p (30, ph::make (6, 6, 6)), N == 1);
  ASSERT_TRUE (multiple_p (0, ph::make (5, 4, 11)));

  /* Test multiple_p (T, T).  */
  ASSERT_TRUE (multiple_p (ph::make (15, 0, 0),
			   ph::make (5, 0, 0)));
  ASSERT_FALSE (multiple_p (ph::make (16, 0, 0),
			    ph::make (5, 0, 0)));
  ASSERT_FALSE (multiple_p (ph::make (14, 5, 5),
			    ph::make (5, 0, 0)));
  ASSERT_TRUE (multiple_p (ph::make (44, 0, 55),
			   ph::make (11, 0, 0)));
  ASSERT_TRUE (multiple_p (ph::make (30, 30, 0),
			   ph::make (6, 0, 0)));
  ASSERT_TRUE (multiple_p (ph::make (30, 35, 45),
			   ph::make (5, 0, 0)));
  ASSERT_EQ (multiple_p (ph::make (30, 35, 44),
			 ph::make (5, 0, 0)), N <= 2);
  ASSERT_EQ (multiple_p (ph::make (30, 34, 45),
			 ph::make (5, 0, 0)), N == 1);
  ASSERT_TRUE (multiple_p (ph::make (0, 0, 0),
			   ph::make (5, 0, 0)));
  ASSERT_TRUE (multiple_p (ph::make (15, 0, 0),
			   ph::make (5, 0, 0)));
  ASSERT_FALSE (multiple_p (ph::make (16, 0, 0),
			    ph::make (5, 0, 0)));
  ASSERT_FALSE (multiple_p (ph::make (14, 0, 0),
			    ph::make (5, 5, 5)));
  ASSERT_EQ (multiple_p (ph::make (44, 0, 0),
			 ph::make (11, 0, 4)), N <= 2);
  ASSERT_EQ (multiple_p (ph::make (30, 0, 0),
			 ph::make (6, 6, 6)), N == 1);
  ASSERT_TRUE (multiple_p (ph::make (0, 0, 0),
			   ph::make (5, 4, 11)));
  ASSERT_TRUE (multiple_p (ph::make (5, 15, 25),
			   ph::make (1, 3, 5)));
  ASSERT_EQ (multiple_p (ph::make (18, 30, 7),
			 ph::make (6, 10, 2)), N <= 2);
  ASSERT_EQ (multiple_p (ph::make (54, 19, 0),
			 ph::make (9, 3, 0)), N == 1);
  ASSERT_TRUE (multiple_p (ph::make (120, 0, 90),
			   ph::make (12, 0, 9)));
  ASSERT_EQ (multiple_p (ph::make (110, 1, 22),
			 ph::make (10, 0, 2)), N == 1);
  ASSERT_EQ (multiple_p (ph::make (120, -1, 22),
			 ph::make (10, 0, 2)), N == 1);
  ASSERT_EQ (multiple_p (ph::make (130, 0, 26),
			 ph::make (10, 1, 2)), N == 1);
  ASSERT_EQ (multiple_p (ph::make (140, 0, 28),
			 ph::make (10, -1, 2)), N == 1);
  ASSERT_FALSE (multiple_p (ph::make (89, 0, 0),
			    ph::make (11, 0, 0)));
  ASSERT_TRUE (multiple_p (ph::make (88, 0, 0),
			   ph::make (11, 0, 0)));
  ASSERT_FALSE (multiple_p (ph::make (87, 0, 0),
			    ph::make (11, 0, 0)));
  ASSERT_TRUE (multiple_p (ph::make (35, 63, 0),
			   ph::make (5, 9, 0)));
  ASSERT_TRUE (multiple_p (ph::make (0, 0, 0),
			   ph::make (11, -24, 25)));
}

/* Test the 3-operand form of multiple_p for both signed and unsigned C.  */

template<unsigned int N, typename C, typename T>
static void
test_multiple_p_with_result ()
{
  typedef coeff_helper<C> ch;
  typedef poly_helper<T> ph;

  /* Test multiple_p (T, C) -> T.  */
  T multiple;
  ASSERT_TRUE (multiple_p (ph::make (15, 0, 0), 5, &multiple));
  ASSERT_KNOWN_EQ (multiple, ch::make (3));
  ASSERT_FALSE (multiple_p (ph::make (16, 0, 0), 5, &multiple));
  ASSERT_FALSE (multiple_p (ph::make (14, 5, 5), 5, &multiple));
  ASSERT_TRUE (multiple_p (ph::make (44, 0, 55), 11, &multiple));
  ASSERT_KNOWN_EQ (multiple, ph::make (4, 0, 5));
  ASSERT_TRUE (multiple_p (ph::make (30, 30, 0), 6, &multiple));
  ASSERT_KNOWN_EQ (multiple, ph::make (5, 5, 0));
  ASSERT_TRUE (multiple_p (ph::make (30, 35, 45), 5, &multiple));
  ASSERT_KNOWN_EQ (multiple, ph::make (6, 7, 9));
  ASSERT_EQ (multiple_p (ph::make (30, 35, 44), 5, &multiple), N <= 2);
  if (N <= 2)
    ASSERT_KNOWN_EQ (multiple, ph::make (6, 7, 0));
  ASSERT_EQ (multiple_p (ph::make (30, 34, 45), 5, &multiple), N == 1);
  if (N == 1)
    ASSERT_KNOWN_EQ (multiple, ch::make (6));
  ASSERT_TRUE (multiple_p (ph::make (0, 0, 0), 5, &multiple));
  ASSERT_KNOWN_EQ (multiple, ch::make (0));

  /* Test multiple_p (C, T) -> T.  */
  ASSERT_TRUE (multiple_p (15, ph::make (5, 0, 0), &multiple));
  ASSERT_KNOWN_EQ (multiple, ch::make (3));
  ASSERT_FALSE (multiple_p (16, ph::make (5, 0, 0), &multiple));
  ASSERT_FALSE (multiple_p (14, ph::make (5, 5, 5), &multiple));
  ASSERT_EQ (multiple_p (44, ph::make (11, 0, 4), &multiple), N <= 2);
  ASSERT_KNOWN_EQ (multiple, ch::make (N <= 2 ? 4 : 3));
  ASSERT_EQ (multiple_p (30, ph::make (6, 6, 6), &multiple), N == 1);
  ASSERT_KNOWN_EQ (multiple, ch::make (N == 1 ? 5 : N == 2 ? 4 : 3));
  ASSERT_TRUE (multiple_p (0, ph::make (5, 4, 11), &multiple));
  ASSERT_KNOWN_EQ (multiple, ch::make (0));

  /* Test multiple_p (T, T) -> T.  */
  ASSERT_TRUE (multiple_p (ph::make (15, 0, 0),
			   ph::make (5, 0, 0),
			   &multiple));
  ASSERT_KNOWN_EQ (multiple, ch::make (3));
  ASSERT_FALSE (multiple_p (ph::make (16, 0, 0),
			    ph::make (5, 0, 0),
			    &multiple));
  ASSERT_FALSE (multiple_p (ph::make (14, 5, 5),
			    ph::make (5, 0, 0),
			    &multiple));
  ASSERT_TRUE (multiple_p (ph::make (44, 0, 55),
			   ph::make (11, 0, 0),
			   &multiple));
  ASSERT_KNOWN_EQ (multiple, ph::make (4, 0, 5));
  ASSERT_TRUE (multiple_p (ph::make (30, 30, 0),
			   ph::make (6, 0, 0),
			   &multiple));
  ASSERT_KNOWN_EQ (multiple, ph::make (5, 5, 0));
  ASSERT_TRUE (multiple_p (ph::make (30, 35, 45),
			   ph::make (5, 0, 0),
			   &multiple));
  ASSERT_KNOWN_EQ (multiple, ph::make (6, 7, 9));
  ASSERT_EQ (multiple_p (ph::make (30, 35, 44),
			 ph::make (5, 0, 0),
			 &multiple), N <= 2);
  if (N <= 2)
    ASSERT_KNOWN_EQ (multiple, ph::make (6, 7, 0));
  ASSERT_EQ (multiple_p (ph::make (30, 34, 45),
			 ph::make (5, 0, 0),
			 &multiple), N == 1);
  if (N == 1)
    ASSERT_KNOWN_EQ (multiple, ch::make (6));
  ASSERT_TRUE (multiple_p (ph::make (0, 0, 0),
			   ph::make (5, 0, 0),
			   &multiple));
  ASSERT_KNOWN_EQ (multiple, ch::make (0));
  ASSERT_TRUE (multiple_p (ph::make (15, 0, 0),
			   ph::make (5, 0, 0),
			   &multiple));
  ASSERT_KNOWN_EQ (multiple, ch::make (3));
  ASSERT_FALSE (multiple_p (ph::make (16, 0, 0),
			    ph::make (5, 0, 0),
			    &multiple));
  ASSERT_FALSE (multiple_p (ph::make (14, 0, 0),
			    ph::make (5, 5, 5),
			    &multiple));
  ASSERT_EQ (multiple_p (ph::make (44, 0, 0),
			 ph::make (11, 0, 4),
			 &multiple), N <= 2);
  if (N <= 2)
    ASSERT_KNOWN_EQ (multiple, ch::make (4));
  ASSERT_EQ (multiple_p (ph::make (30, 0, 0),
			 ph::make (6, 6, 6),
			 &multiple), N == 1);
  if (N == 1)
    ASSERT_KNOWN_EQ (multiple, ch::make (5));
  ASSERT_TRUE (multiple_p (ph::make (0, 0, 0),
			   ph::make (5, 4, 11),
			   &multiple));
  ASSERT_KNOWN_EQ (multiple, ch::make (0));
  ASSERT_TRUE (multiple_p (ph::make (5, 15, 25),
			   ph::make (1, 3, 5),
			   &multiple));
  ASSERT_KNOWN_EQ (multiple, ch::make (5));
  ASSERT_EQ (multiple_p (ph::make (18, 30, 7),
			 ph::make (6, 10, 2),
			 &multiple), N <= 2);
  if (N <= 2)
    ASSERT_KNOWN_EQ (multiple, ch::make (3));
  ASSERT_EQ (multiple_p (ph::make (54, 19, 0),
			 ph::make (9, 3, 0),
			 &multiple), N == 1);
  if (N == 1)
    ASSERT_KNOWN_EQ (multiple, ch::make (6));
  ASSERT_TRUE (multiple_p (ph::make (120, 0, 90),
			   ph::make (12, 0, 9),
			   &multiple));
  ASSERT_KNOWN_EQ (multiple, ch::make (10));
  ASSERT_EQ (multiple_p (ph::make (110, 1, 22),
			 ph::make (10, 0, 2),
			 &multiple), N == 1);
  ASSERT_KNOWN_EQ (multiple, ch::make (N == 1 ? 11 : 10));
  ASSERT_EQ (multiple_p (ph::make (120, -1, 22),
			 ph::make (10, 0, 2),
			 &multiple), N == 1);
  ASSERT_KNOWN_EQ (multiple, ch::make (N == 1 ? 12 : 10));
  ASSERT_EQ (multiple_p (ph::make (130, 0, 26),
			 ph::make (10, 1, 2),
			 &multiple), N == 1);
  ASSERT_KNOWN_EQ (multiple, ch::make (N == 1 ? 13 : 10));
  ASSERT_EQ (multiple_p (ph::make (140, 0, 28),
			 ph::make (10, -1, 2),
			 &multiple), N == 1);
  ASSERT_KNOWN_EQ (multiple, ch::make (N == 1 ? 14 : 10));
  ASSERT_FALSE (multiple_p (ph::make (89, 0, 0),
			    ph::make (11, 0, 0),
			    &multiple));
  ASSERT_TRUE (multiple_p (ph::make (88, 0, 0),
			   ph::make (11, 0, 0),
			   &multiple));
  ASSERT_KNOWN_EQ (multiple, ch::make (8));
  ASSERT_FALSE (multiple_p (ph::make (87, 0, 0),
			    ph::make (11, 0, 0),
			    &multiple));
  ASSERT_TRUE (multiple_p (ph::make (35, 63, 0),
			   ph::make (5, 9, 0),
			   &multiple));
  ASSERT_KNOWN_EQ (multiple, ch::make (7));
  ASSERT_TRUE (multiple_p (ph::make (0, 0, 0),
			   ph::make (11, -24, 25),
			   &multiple));
  ASSERT_KNOWN_EQ (multiple, ch::make (0));
}

/* Test exact_div for both signed and unsigned C.  */

template<unsigned int N, typename C, typename T>
static void
test_exact_div ()
{
  typedef coeff_helper<C> ch;
  typedef poly_helper<T> ph;

  /* Test exact_div (T, C).  */
  ASSERT_KNOWN_EQ (exact_div (ph::make (15, 0, 0), 5),
		   ch::make (3));
  ASSERT_KNOWN_EQ (exact_div (ph::make (44, 0, 55), 11),
		   ph::make (4, 0, 5));
  ASSERT_KNOWN_EQ (exact_div (ph::make (30, 30, 0), 6),
		   ph::make (5, 5, 0));
  ASSERT_KNOWN_EQ (exact_div (ph::make (30, 35, 45), 5),
		   ph::make (6, 7, 9));
  ASSERT_KNOWN_EQ (exact_div (ph::make (0, 0, 0), 5),
		   ch::make (0));

  /* Test exact_div (T, T).  */
  ASSERT_KNOWN_EQ (exact_div (ph::make (15, 0, 0),
			      ph::make (5, 0, 0)),
		   ch::make (3));
  ASSERT_KNOWN_EQ (exact_div (ph::make (44, 0, 55),
			      ph::make (11, 0, 0)),
		   ph::make (4, 0, 5));
  ASSERT_KNOWN_EQ (exact_div (ph::make (30, 30, 0),
			      ph::make (6, 0, 0)),
		   ph::make (5, 5, 0));
  ASSERT_KNOWN_EQ (exact_div (ph::make (30, 35, 45),
			      ph::make (5, 0, 0)),
		   ph::make (6, 7, 9));
  ASSERT_KNOWN_EQ (exact_div (ph::make (0, 0, 0),
			      ph::make (5, 0, 0)),
		   ch::make (0));
  ASSERT_KNOWN_EQ (exact_div (ph::make (15, 0, 0),
			      ph::make (5, 0, 0)),
		   ch::make (3));
  ASSERT_KNOWN_EQ (exact_div (ph::make (0, 0, 0),
			      ph::make (5, 4, 11)),
		   ch::make (0));
  ASSERT_KNOWN_EQ (exact_div (ph::make (5, 15, 25),
			      ph::make (1, 3, 5)),
		   ch::make (5));
  ASSERT_KNOWN_EQ (exact_div (ph::make (120, 0, 90),
			      ph::make (12, 0, 9)),
		   ch::make (10));
  ASSERT_KNOWN_EQ (exact_div (ph::make (88, 0, 0),
			      ph::make (11, 0, 0)),
		   ch::make (8));
  ASSERT_KNOWN_EQ (exact_div (ph::make (35, 63, 0),
			      ph::make (5, 9, 0)),
		   ch::make (7));
  ASSERT_KNOWN_EQ (exact_div (ph::make (0, 0, 0),
			      ph::make (11, -24, 25)),
		   ch::make (0));
}

/* Test the form of can_div_trunc_p that returns a constant quotient,
   for both signed and unsigned C.  */

template<unsigned int N, typename C, typename T>
static void
test_can_div_trunc_p_const ()
{
  typedef coeff_helper<C> ch;
  typedef poly_helper<T> ph;

  /* Test can_div_trunc_p (T, C) -> C.  */
  C const_quot;
  ASSERT_TRUE (can_div_trunc_p (ph::make (22, 0, 0), 5, &const_quot));
  ASSERT_KNOWN_EQ (const_quot, C (4));
  ASSERT_EQ (can_div_trunc_p (ph::make (44, 0, 1), 5, &const_quot), N <= 2);
  ASSERT_KNOWN_EQ (const_quot, C (N <= 2 ? 8 : 4));
  ASSERT_EQ (can_div_trunc_p (ph::make (88, 1, 0), 5, &const_quot), N == 1);
  ASSERT_KNOWN_EQ (const_quot, C (N == 1 ? 17 : N == 2 ? 8 : 4));
  ASSERT_TRUE (can_div_trunc_p (ph::make (20, 0, 0), 5, &const_quot));
  ASSERT_KNOWN_EQ (const_quot, C (4));
  ASSERT_TRUE (can_div_trunc_p (ph::make (19, 0, 0), 5, &const_quot));
  ASSERT_KNOWN_EQ (const_quot, C (3));

  /* Test can_div_trunc_p (T, T) -> C.  */
  ASSERT_TRUE (can_div_trunc_p (ph::make (8, 44, 28),
				ph::make (2, 11, 7),
				&const_quot));
  ASSERT_EQ (const_quot, C (4));
  ASSERT_TRUE (can_div_trunc_p (ph::make (9, 23, 30),
				ph::make (4, 8, 12),
				&const_quot));
  ASSERT_EQ (const_quot, C (2));
  ASSERT_EQ (can_div_trunc_p (ph::make (15, 25, 40),
			      ph::make (4, 8, 10),
			      &const_quot), N <= 2);
  ASSERT_EQ (const_quot, C (N <= 2 ? 3 : 2));
  ASSERT_EQ (can_div_trunc_p (ph::make (43, 79, 80),
			      ph::make (4, 8, 10),
			      &const_quot), N == 1);
  ASSERT_EQ (const_quot, C (N == 1 ? 10 : N == 2 ? 3 : 2));
  ASSERT_TRUE (can_div_trunc_p (ph::make (3, 4, 5),
				ph::make (4, 5, 6),
				&const_quot));
  ASSERT_EQ (const_quot, C (0));
  ASSERT_TRUE (can_div_trunc_p (ph::make (3, 4, 6),
				ph::make (4, 5, 6),
				&const_quot));
  ASSERT_EQ (const_quot, C (0));
  ASSERT_TRUE (can_div_trunc_p (ph::make (3, 5, 5),
				ph::make (4, 5, 6),
				&const_quot));
  ASSERT_EQ (const_quot, C (0));
  ASSERT_EQ (can_div_trunc_p (ph::make (3, 4, 7),
			      ph::make (4, 5, 6),
			      &const_quot), N <= 2);
  ASSERT_EQ (const_quot, C (0));
  ASSERT_EQ (can_div_trunc_p (ph::make (3, 6, 0),
			      ph::make (4, 5, 6),
			      &const_quot), N == 1);
  ASSERT_EQ (const_quot, C (0));
  ASSERT_TRUE (can_div_trunc_p (ph::make (56, 0, 11),
				ph::make (11, 0, 2),
				&const_quot));
  ASSERT_EQ (const_quot, C (5));
  ASSERT_EQ (can_div_trunc_p (ph::make (66, 1, 12),
			      ph::make (11, 0, 2),
			      &const_quot), N == 1);
  ASSERT_EQ (const_quot, C (N == 1 ? 6 : 5));
  ASSERT_EQ (can_div_trunc_p (ph::make (77, -1, 14),
			      ph::make (11, 0, 2),
			      &const_quot), N == 1);
  ASSERT_EQ (const_quot, C (N == 1 ? 7 : 5));
  ASSERT_TRUE (can_div_trunc_p (ph::make (89, 0, 0),
				ph::make (11, 0, 0),
				&const_quot));
  ASSERT_EQ (const_quot, C (8));
  ASSERT_EQ (can_div_trunc_p (ph::make (101, 0, 1),
			      ph::make (11, 0, 0),
			      &const_quot), N <= 2);
  ASSERT_EQ (const_quot, C (N <= 2 ? 9 : 8));
  ASSERT_TRUE (can_div_trunc_p (ph::make (0, 0, 0),
				ph::make (4, 5, 6),
				&const_quot));
  ASSERT_EQ (const_quot, C (0));

  /* Test can_div_trunc_p (T, T) -> C, T.  */
  T rem;
  ASSERT_TRUE (can_div_trunc_p (ph::make (8, 44, 28),
				ph::make (2, 11, 7),
				&const_quot, &rem));
  ASSERT_EQ (const_quot, C (4));
  ASSERT_KNOWN_EQ (rem, ch::make (0));
  ASSERT_TRUE (can_div_trunc_p (ph::make (9, 23, 30),
				ph::make (4, 8, 12),
				&const_quot, &rem));
  ASSERT_EQ (const_quot, C (2));
  ASSERT_KNOWN_EQ (rem, ph::make (1, 7, 6));
  ASSERT_EQ (can_div_trunc_p (ph::make (15, 25, 40),
			      ph::make (4, 8, 10),
			      &const_quot, &rem), N <= 2);
  ASSERT_EQ (const_quot, C (N <= 2 ? 3 : 2));
  if (N <= 2)
    ASSERT_KNOWN_EQ (rem, ph::make (3, 1, 0));
  ASSERT_EQ (can_div_trunc_p (ph::make (43, 79, 80),
			      ph::make (4, 8, 10),
			      &const_quot, &rem), N == 1);
  ASSERT_EQ (const_quot, C (N == 1 ? 10 : N == 2 ? 3 : 2));
  if (N == 1)
    ASSERT_KNOWN_EQ (rem, ch::make (3));
  ASSERT_TRUE (can_div_trunc_p (ph::make (3, 4, 5),
				ph::make (4, 5, 6),
				&const_quot, &rem));
  ASSERT_EQ (const_quot, C (0));
  ASSERT_KNOWN_EQ (rem, ph::make (3, 4, 5));
  ASSERT_TRUE (can_div_trunc_p (ph::make (3, 4, 6),
				ph::make (4, 5, 6),
				&const_quot, &rem));
  ASSERT_EQ (const_quot, C (0));
  ASSERT_KNOWN_EQ (rem, ph::make (3, 4, 6));
  ASSERT_TRUE (can_div_trunc_p (ph::make (3, 5, 5),
				ph::make (4, 5, 6),
				&const_quot, &rem));
  ASSERT_EQ (const_quot, C (0));
  ASSERT_KNOWN_EQ (rem, ph::make (3, 5, 5));
  ASSERT_TRUE (can_div_trunc_p (ph::make (56, 0, 11),
				ph::make (11, 0, 2),
				&const_quot, &rem));
  ASSERT_EQ (const_quot, C (5));
  ASSERT_KNOWN_EQ (rem, ph::make (1, 0, 1));
  ASSERT_EQ (can_div_trunc_p (ph::make (66, 1, 12),
			      ph::make (11, 0, 2),
			      &const_quot, &rem), N == 1);
  ASSERT_EQ (const_quot, C (N == 1 ? 6 : 5));
  if (N == 1)
    ASSERT_KNOWN_EQ (rem, ch::make (0));
  ASSERT_EQ (can_div_trunc_p (ph::make (77, -1, 14),
			      ph::make (11, 0, 2),
			      &const_quot, &rem), N == 1);
  ASSERT_EQ (const_quot, C (N == 1 ? 7 : 5));
  if (N == 1)
    ASSERT_KNOWN_EQ (rem, ch::make (0));
  ASSERT_TRUE (can_div_trunc_p (ph::make (89, 0, 0),
				ph::make (11, 0, 0),
				&const_quot, &rem));
  ASSERT_EQ (const_quot, C (8));
  ASSERT_KNOWN_EQ (rem, ch::make (1));
  ASSERT_EQ (can_div_trunc_p (ph::make (101, 0, 1),
			      ph::make (11, 0, 0),
			      &const_quot, &rem), N <= 2);
  ASSERT_EQ (const_quot, C (N <= 2 ? 9 : 8));
  if (N <= 2)
    ASSERT_KNOWN_EQ (rem, ch::make (2));
  ASSERT_TRUE (can_div_trunc_p (ph::make (0, 0, 0),
				ph::make (4, 5, 6),
				&const_quot, &rem));
  ASSERT_EQ (const_quot, C (0));
  ASSERT_KNOWN_EQ (rem, ch::make (0));
}

/* Test the form of can_div_trunc_p that returns a polynomail quotient,
   for both signed and unsigned C.  */

template<unsigned int N, typename C, typename T>
static void
test_can_div_trunc_p_poly ()
{
  typedef coeff_helper<C> ch;
  typedef poly_helper<T> ph;

  /* Test can_div_trunc_p (T, C) -> T.  */
  T quot;
  ASSERT_TRUE (can_div_trunc_p (ph::make (22, 0, 0), 5, &quot));
  ASSERT_KNOWN_EQ (quot, ch::make (4));
  ASSERT_TRUE (can_div_trunc_p (ph::make (45, 40, 24), 4, &quot));
  ASSERT_KNOWN_EQ (quot, ph::make (11, 10, 6));
  ASSERT_EQ (can_div_trunc_p (ph::make (13, 18, 19), 6, &quot), N <= 2);
  if (N <= 2)
    ASSERT_KNOWN_EQ (quot, ph::make (2, 3, 0));
  ASSERT_EQ (can_div_trunc_p (ph::make (55, 11, 10), 10, &quot), N == 1);
  if (N == 1)
    ASSERT_KNOWN_EQ (quot, ch::make (5));

  /* Test can_div_trunc_p (T, C) -> T, C.  */
  C const_rem;
  ASSERT_TRUE (can_div_trunc_p (ph::make (22, 0, 0), 5,
				&quot, &const_rem));
  ASSERT_KNOWN_EQ (quot, ch::make (4));
  ASSERT_EQ (const_rem, C (2));
  ASSERT_TRUE (can_div_trunc_p (ph::make (45, 40, 24), 4,
				&quot, &const_rem));
  ASSERT_KNOWN_EQ (quot, ph::make (11, 10, 6));
  ASSERT_EQ (const_rem, C (1));
  ASSERT_EQ (can_div_trunc_p (ph::make (13, 18, 19), 6,
			      &quot, &const_rem), N <= 2);
  if (N <= 2)
    {
      ASSERT_KNOWN_EQ (quot, ph::make (2, 3, 0));
      ASSERT_EQ (const_rem, C (1));
    }
  ASSERT_EQ (can_div_trunc_p (ph::make (55, 11, 10), 10,
			      &quot, &const_rem), N == 1);
  if (N == 1)
    {
      ASSERT_KNOWN_EQ (quot, ch::make (5));
      ASSERT_EQ (const_rem, C (5));
    }
}

/* Test can_div_away_from_zero_p for both signed and unsigned C.  */

template<unsigned int N, typename C, typename T>
static void
test_can_div_away_from_zero_p ()
{
  typedef poly_helper<T> ph;

  /* Test can_div_away_from_zero_p (T, T) -> C.  */
  C const_quot;
  ASSERT_TRUE (can_div_away_from_zero_p (ph::make (8, 44, 28),
					 ph::make (2, 11, 7),
					 &const_quot));
  ASSERT_EQ (const_quot, C (4));
  ASSERT_TRUE (can_div_away_from_zero_p (ph::make (9, 23, 30),
					 ph::make (4, 8, 12),
					 &const_quot));
  ASSERT_EQ (const_quot, C (3));
  ASSERT_EQ (can_div_away_from_zero_p (ph::make (15, 25, 40),
				       ph::make (4, 8, 10),
				       &const_quot), N <= 2);
  ASSERT_EQ (const_quot, C (N <= 2 ? 4 : 3));
  ASSERT_EQ (can_div_away_from_zero_p (ph::make (43, 79, 80),
				       ph::make (4, 8, 10),
				       &const_quot), N == 1);
  ASSERT_EQ (const_quot, C (N == 1 ? 11 : N == 2 ? 4 : 3));
  ASSERT_TRUE (can_div_away_from_zero_p (ph::make (3, 4, 5),
					 ph::make (4, 5, 6),
					 &const_quot));
  ASSERT_EQ (const_quot, C (1));
  ASSERT_TRUE (can_div_away_from_zero_p (ph::make (3, 4, 6),
					 ph::make (4, 5, 6),
					 &const_quot));
  ASSERT_EQ (const_quot, C (1));
  ASSERT_TRUE (can_div_away_from_zero_p (ph::make (3, 5, 5),
					 ph::make (4, 5, 6),
					 &const_quot));
  ASSERT_EQ (const_quot, C (1));
  ASSERT_TRUE (can_div_away_from_zero_p (ph::make (56, 0, 11),
					 ph::make (11, 0, 2),
					 &const_quot));
  ASSERT_EQ (const_quot, C (6));
  ASSERT_EQ (can_div_away_from_zero_p (ph::make (66, 1, 12),
				       ph::make (11, 0, 2),
				       &const_quot), N == 1);
  ASSERT_EQ (const_quot, C (6));
  ASSERT_EQ (can_div_away_from_zero_p (ph::make (77, -1, 14),
				       ph::make (11, 0, 2),
				       &const_quot), N == 1);
  ASSERT_EQ (const_quot, C (N == 1 ? 7 : 6));
  ASSERT_TRUE (can_div_away_from_zero_p (ph::make (89, 0, 0),
					 ph::make (11, 0, 0),
					 &const_quot));
  ASSERT_EQ (const_quot, C (9));
  ASSERT_EQ (can_div_away_from_zero_p (ph::make (101, 0, 1),
				       ph::make (11, 0, 0),
				       &const_quot), N <= 2);
  ASSERT_EQ (const_quot, C (N <= 2 ? 10 : 9));
  ASSERT_TRUE (can_div_away_from_zero_p (ph::make (0, 0, 0),
					 ph::make (4, 5, 6),
					 &const_quot));
  ASSERT_EQ (const_quot, C (0));
}

/* Test known_size_p.  */

template<unsigned int N, typename C, typename T>
static void
test_known_size_p ()
{
  typedef poly_helper<T> ph;

  ASSERT_EQ (known_size_p (ph::make (-1, 0, -1)), N == 3);
  ASSERT_EQ (known_size_p (ph::make (-1, -1, 0)), N >= 2);
  ASSERT_EQ (known_size_p (ph::make (-1, -1, -1)), N >= 2);
  ASSERT_FALSE (known_size_p (ph::make (-1, 0, 0)));
  ASSERT_TRUE (known_size_p (ph::make (0, 0, 0)));
  ASSERT_TRUE (known_size_p (ph::make (1, 0, 0)));
}

/* Test maybe_in_range_p for both signed and unsigned C.  */

template<unsigned int N, typename C, typename T>
static void
test_maybe_in_range_p ()
{
  typedef coeff_helper<C> ch;
  typedef poly_helper<T> ph;

  ASSERT_FALSE (maybe_in_range_p (ch::make (4),
				  ph::make (5, 1, 2),
				  ch::make (-1)));
  ASSERT_FALSE (maybe_in_range_p (ph::make (4, 0, 0),
				  ph::make (5, 1, 2),
				  ch::make (-1)));
  ASSERT_FALSE (maybe_in_range_p (ph::make (4, 1, 2),
				  ph::make (5, 1, 2),
				  ch::make (-1)));
  ASSERT_TRUE (maybe_in_range_p (ph::make (5, 1, 2),
				 ph::make (5, 1, 2),
				 ch::make (-1)));
  ASSERT_EQ (maybe_in_range_p (ph::make (4, 0, 3),
			       ph::make (5, 1, 2),
			       ch::make (-1)), N == 3);
  ASSERT_EQ (maybe_in_range_p (ph::make (4, 2, 0),
			       ph::make (5, 1, 2),
			       ch::make (-1)), N >= 2);
  ASSERT_TRUE (maybe_in_range_p (ph::make (500, 100, 200),
				 ph::make (5, 1, 2),
				 ch::make (-1)));
  ASSERT_EQ (maybe_in_range_p (ph::make (6, 1, 0),
			       ph::make (5, 1, 1),
			       ch::make (1)), N == 3);
  ASSERT_EQ (maybe_in_range_p (ph::make (6, 0, 1),
			       ph::make (5, 1, 1),
			       ch::make (1)), N >= 2);
  ASSERT_FALSE (maybe_in_range_p (ph::make (14, 1, 2),
				  ph::make (5, 1, 2),
				  ch::make (9)));
  ASSERT_FALSE (maybe_in_range_p (ph::make (14, 1, 2),
				  ch::make (5),
				  ph::make (9, 1, 2)));
  ASSERT_FALSE (maybe_in_range_p (ph::make (15, 15, 17),
				  ph::make (8, 10, 11),
				  ph::make (7, 5, 6)));
  ASSERT_EQ (maybe_in_range_p (ph::make (15, 15, 16),
			       ph::make (8, 10, 11),
			       ph::make (7, 5, 6)), N == 3);
  ASSERT_EQ (maybe_in_range_p (ph::make (15, 14, 17),
			       ph::make (8, 10, 11),
			       ph::make (7, 5, 6)), N >= 2);
  ASSERT_TRUE (maybe_in_range_p (ph::make (6, 100, 1000),
				 ph::make (5, 10, 11),
				 ph::make (2, 1, 2)));
  ASSERT_FALSE (maybe_in_range_p (ph::make (6, 8, 2),
				  ph::make (6, 8, 2),
				  ch::make (0)));
  ASSERT_EQ (maybe_in_range_p (ph::make (6, 8, 1),
			       ph::make (6, 7, 2),
			       ph::make (0, 1, 2)), N == 3);
}

/* Test known_in_range_p for both signed and unsigned C.  */

template<unsigned int N, typename C, typename T>
static void
test_known_in_range_p ()
{
  typedef coeff_helper<C> ch;
  typedef poly_helper<T> ph;

  ASSERT_FALSE (known_in_range_p (ch::make (4),
				  ph::make (5, 1, 2),
				  ch::make (-1)));
  ASSERT_FALSE (known_in_range_p (ph::make (5, 1, 2),
				  ph::make (5, 1, 2),
				  ch::make (-1)));
  ASSERT_FALSE (known_in_range_p (ph::make (6, 2, 3),
				  ph::make (5, 1, 2),
				  ch::make (-1)));
  ASSERT_FALSE (known_in_range_p (ph::make (6, 1, 0),
				  ph::make (5, 1, 1),
				  ch::make (1)));
  ASSERT_FALSE (known_in_range_p (ph::make (6, 0, 1),
				  ph::make (5, 1, 1),
				  ch::make (1)));
  ASSERT_EQ (known_in_range_p (ph::make (6, 1, 0),
			       ph::make (5, 1, 1),
			       ch::make (2)), N <= 2);
  ASSERT_EQ (known_in_range_p (ph::make (6, 0, 1),
			       ph::make (5, 1, 1),
			       ch::make (2)), N == 1);
  ASSERT_TRUE (known_in_range_p (ph::make (6, 4, 5),
				 ph::make (5, 1, 2),
				 ph::make (2, 3, 3)));
  ASSERT_EQ (known_in_range_p (ph::make (6, 4, 6),
			       ph::make (5, 1, 2),
			       ph::make (2, 3, 3)), N <= 2);
  ASSERT_EQ (known_in_range_p (ph::make (6, 5, 5),
			       ph::make (5, 1, 2),
			       ph::make (2, 3, 3)), N == 1);
  ASSERT_FALSE (known_in_range_p (ph::make (6, 8, 2),
				  ph::make (6, 8, 2),
				  ch::make (0)));
  ASSERT_FALSE (known_in_range_p (ph::make (6, 8, 1),
				  ph::make (6, 7, 2),
				  ph::make (0, 1, 2)));
}

/* Test ranges_maybe_overlap_p for both signed and unsigned C.  */

template<unsigned int N, typename C, typename T>
static void
test_ranges_maybe_overlap_p ()
{
  typedef coeff_helper<C> ch;
  typedef poly_helper<T> ph;

  ASSERT_TRUE (ranges_maybe_overlap_p (ph::make (4, 1, 2),
				       ch::make (-1),
				       ph::make (500, 3, 5),
				       ch::make (1)));
  ASSERT_FALSE (ranges_maybe_overlap_p (ph::make (100, 1, 5),
					ch::make (-1),
					ph::make (50, 1, 5),
					ch::make (50)));
  ASSERT_EQ (ranges_maybe_overlap_p (ph::make (100, 1, 5),
				     ch::make (-1),
				     ph::make (50, 0, 6),
				     ch::make (50)), N == 3);
  ASSERT_EQ (ranges_maybe_overlap_p (ph::make (100, 1, 5),
				     ch::make (-1),
				     ph::make (50, 2, 0),
				     ch::make (50)), N >= 2);
  ASSERT_TRUE (ranges_maybe_overlap_p (ph::make (500, 3, 5),
				       ch::make (1),
				       ph::make (4, 1, 2),
				       ch::make (-1)));
  ASSERT_FALSE (ranges_maybe_overlap_p (ph::make (50, 1, 5),
					ch::make (50),
					ph::make (100, 1, 5),
					ch::make (-1)));
  ASSERT_FALSE (ranges_maybe_overlap_p (ph::make (10, 2, 3),
					ch::make (0),
					ch::make (0),
					ph::make (20, 30, 40)));
  ASSERT_EQ (ranges_maybe_overlap_p (ph::make (10, 2, 3),
				     ph::make (0, 1, 1),
				     ph::make (0, 1, 1),
				     ph::make (20, 30, 40)), N >= 2);
  ASSERT_FALSE (ranges_maybe_overlap_p (ch::make (0),
					ph::make (20, 30, 40),
					ph::make (10, 2, 3),
					ch::make (0)));
  ASSERT_EQ (ranges_maybe_overlap_p (ph::make (0, 1, 1),
				     ph::make (20, 30, 40),
				     ph::make (10, 2, 3),
				     ph::make (0, 1, 0)), N >= 2);
  ASSERT_TRUE (ranges_maybe_overlap_p (ph::make (8, 10, 15),
				       ph::make (2, 6, 20),
				       ch::make (7),
				       ch::make (2)));
  ASSERT_FALSE (ranges_maybe_overlap_p (ph::make (8, 10, 15),
					ph::make (2, 6, 20),
					ch::make (6),
					ph::make (2, 10, 15)));
  ASSERT_EQ (ranges_maybe_overlap_p (ph::make (8, 10, 15),
				     ph::make (2, 6, 20),
				     ch::make (6),
				     ph::make (0, 0, 16)), N == 3);
  ASSERT_EQ (ranges_maybe_overlap_p (ph::make (8, 10, 15),
				     ph::make (2, 6, 20),
				     ch::make (6),
				     ph::make (0, 11, 0)), N >= 2);
  ASSERT_FALSE (ranges_maybe_overlap_p (ph::make (80, 4, 5),
					ph::make (10, 6, 7),
					ph::make (100, 10, 12),
					ph::make (20, 1, 2)));
  ASSERT_EQ (ranges_maybe_overlap_p (ph::make (80, 4, 5),
				     ph::make (10, 6, 7),
				     ph::make (100, 10, 11),
				     ph::make (0, 0, 2)), N == 3);
  ASSERT_EQ (ranges_maybe_overlap_p (ph::make (80, 5, 5),
				     ph::make (0, 6, 0),
				     ph::make (100, 10, 12),
				     ph::make (20, 1, 2)), N >= 2);
}

/* Test ranges_known_overlap_p for both signed and unsigned C.  */

template<unsigned int N, typename C, typename T>
static void
test_ranges_known_overlap_p ()
{
  typedef coeff_helper<C> ch;
  typedef poly_helper<T> ph;

  ASSERT_FALSE (ranges_known_overlap_p (ph::make (5, 1, 2),
					ch::make (-1),
					ch::make (4),
					ch::make (2)));
  ASSERT_FALSE (ranges_known_overlap_p (ch::make (9),
					ph::make (2, 3, 4),
					ch::make (10),
					ch::make (-1)));
  ASSERT_FALSE (ranges_known_overlap_p (ph::make (10, 2, 3),
					ch::make (0),
					ch::make (0),
					ph::make (20, 30, 40)));
  ASSERT_FALSE (ranges_known_overlap_p (ph::make (10, 2, 3),
					ph::make (0, 1, 1),
					ph::make (0, 1, 1),
					ph::make (20, 30, 40)));
  ASSERT_FALSE (ranges_known_overlap_p (ch::make (0),
					ph::make (20, 30, 40),
					ph::make (10, 2, 3),
					ch::make (0)));
  ASSERT_FALSE (ranges_known_overlap_p (ph::make (0, 1, 1),
					ph::make (20, 30, 40),
					ph::make (10, 2, 3),
					ph::make (0, 1, 0)));
  ASSERT_EQ (ranges_known_overlap_p (ph::make (5, 1, 2),
				     ch::make (1),
				     ch::make (4),
				     ch::make (2)), N == 1);
  ASSERT_TRUE (ranges_known_overlap_p (ph::make (5, 1, 2),
				       ch::make (1),
				       ph::make (4, 1, 2),
				       ch::make (2)));
  ASSERT_TRUE (ranges_known_overlap_p (ch::make (9),
				       ph::make (2, 3, 4),
				       ch::make (10),
				       ch::make (1)));
  ASSERT_FALSE (ranges_known_overlap_p (ph::make (10, 11, 12),
					ph::make (20, 30, 40),
					ch::make (30),
					ch::make (1)));
  ASSERT_TRUE (ranges_known_overlap_p (ph::make (10, 11, 12),
				       ph::make (20, 30, 40),
				       ph::make (29, 41, 52),
				       ch::make (1)));
  ASSERT_EQ (ranges_known_overlap_p (ph::make (10, 11, 12),
				     ph::make (20, 30, 40),
				     ph::make (29, 41, 53),
				     ch::make (1)), N <= 2);
  ASSERT_EQ (ranges_known_overlap_p (ph::make (10, 11, 12),
				     ph::make (20, 30, 40),
				     ph::make (29, 42, 52),
				     ch::make (1)), N == 1);
  ASSERT_TRUE (ranges_known_overlap_p (ph::make (29, 41, 52),
				       ch::make (1),
				       ph::make (10, 11, 12),
				       ph::make (20, 30, 40)));
  ASSERT_EQ (ranges_known_overlap_p (ph::make (29, 41, 53),
				     ch::make (1),
				     ph::make (10, 11, 12),
				     ph::make (20, 30, 40)), N <= 2);
  ASSERT_EQ (ranges_known_overlap_p (ph::make (29, 42, 52),
				     ch::make (1),
				     ph::make (10, 11, 12),
				     ph::make (20, 30, 40)), N == 1);
  ASSERT_TRUE (ranges_known_overlap_p (ph::make (10, 0, 20),
				       ph::make (4, 4, 4),
				       ph::make (7, 3, 20),
				       ph::make (4, 4, 4)));
}

/* Test known_subrange_p for both signed and unsigned C.  */

template<unsigned int N, typename C, typename T>
static void
test_known_subrange_p ()
{
  typedef coeff_helper<C> ch;
  typedef poly_helper<T> ph;

  ASSERT_FALSE (known_subrange_p (ph::make (5, 1, 2),
				  ch::make (-1),
				  ch::make (4),
				  ph::make (2, 2, 2)));
  ASSERT_FALSE (known_subrange_p (ph::make (5, 2, 3),
				  ch::make (2),
				  ph::make (4, 1, 2),
				  ch::make (-1)));
  ASSERT_FALSE (known_subrange_p (ph::make (6, 2, 3),
				  ph::make (0, 1, 1),
				  ch::make (4),
				  ph::make (3, 4, 11)));
  ASSERT_TRUE (known_subrange_p (ph::make (6, 2, 3),
				 ph::make (1, 1, 1),
				 ch::make (4),
				 ph::make (3, 4, 11)));
  ASSERT_FALSE (known_subrange_p (ph::make (6, 2, 3),
				  ph::make (1, 1, 1),
				  ch::make (4),
				  ph::make (2, 4, 11)));
  ASSERT_TRUE (known_subrange_p (ph::make (10, 20, 30),
				 ph::make (5, 6, 7),
				 ph::make (9, 19, 29),
				 ph::make (6, 7, 8)));
  ASSERT_EQ (known_subrange_p (ph::make (10, 20, 31),
			       ph::make (5, 6, 7),
			       ph::make (9, 19, 29),
			       ph::make (6, 7, 8)), N <= 2);
  ASSERT_EQ (known_subrange_p (ph::make (10, 20, 30),
			       ph::make (5, 7, 7),
			       ph::make (9, 19, 29),
			       ph::make (6, 7, 8)), N == 1);
  ASSERT_EQ (known_subrange_p (ph::make (10, 20, 30),
			       ph::make (5, 6, 7),
			       ph::make (9, 18, 29),
			       ph::make (6, 7, 8)), N == 1);
  ASSERT_EQ (known_subrange_p (ph::make (10, 20, 30),
			       ph::make (5, 6, 7),
			       ph::make (9, 19, 29),
			       ph::make (6, 6, 8)), N == 1);
}

/* Test coeffs_in_range_p for both signed and unsigned C.  */

template<unsigned int N, typename C, typename T>
static void
test_coeffs_in_range_p (void)
{
  typedef coeff_helper<C> ch;
  typedef poly_helper<T> ph;

  ASSERT_TRUE (coeffs_in_range_p (ph::make (10, 20, 30), 10, 30));
  ASSERT_EQ (coeffs_in_range_p (ph::make (1, 10, 19), 0, 11), N <= 2);
  ASSERT_EQ (coeffs_in_range_p (ph::make (100, 1, 102), 10, 100), N == 1);
  ASSERT_FALSE (coeffs_in_range_p (ph::make (10, 11, 12), 7, 9));
  ASSERT_FALSE (coeffs_in_range_p (ph::make (10, 11, 12), 13, 15));
}

/* Test maybe_eq for poly_int<2, C>, given that C is signed.  */

template<typename C>
static void
test_signed_maybe_eq_2 ()
{
  typedef poly_int<2, C> T;

  /* Test maybe_eq (T, C).  */
  ASSERT_TRUE (maybe_eq (T (4, -4), 0));
  ASSERT_FALSE (maybe_eq (T (4, -4), 1));
  ASSERT_TRUE (maybe_eq (T (4, -4), 4));
  ASSERT_FALSE (maybe_eq (T (4, -4), 8));
  ASSERT_TRUE (maybe_eq (T (4, -4), -4));
  ASSERT_FALSE (maybe_eq (T (4, -4), -3));

  /* Test maybe_eq (C, T).  */
  ASSERT_FALSE (maybe_eq (0, T (4, -3)));
  ASSERT_TRUE (maybe_eq (1, T (4, -3)));
  ASSERT_TRUE (maybe_eq (4, T (4, -3)));
  ASSERT_FALSE (maybe_eq (7, T (4, -3)));
  ASSERT_FALSE (maybe_eq (T (4, -3), -3));
  ASSERT_TRUE (maybe_eq (T (4, -3), -2));

  /* Test maybe_eq (T, T).  */
  ASSERT_TRUE (maybe_eq (T (0, 3), T (6, 1)));
  ASSERT_FALSE (maybe_eq (T (0, -3), T (6, 1)));
  ASSERT_FALSE (maybe_eq (T (0, 3), T (7, 1)));
  ASSERT_TRUE (maybe_eq (T (-3, 4), T (7, -1)));
  ASSERT_FALSE (maybe_eq (T (-3, 4), T (6, -1)));
}

/* Test known_ne for poly_int<2, C>, given that C is signed.  */

template<typename C>
static void
test_signed_known_ne_2 ()
{
  typedef poly_int<2, C> T;

  /* Test known_ne (T, C).  */
  ASSERT_FALSE (known_ne (T (4, -4), 0));
  ASSERT_TRUE (known_ne (T (4, -4), 1));
  ASSERT_FALSE (known_ne (T (4, -4), 4));
  ASSERT_TRUE (known_ne (T (4, -4), 8));
  ASSERT_FALSE (known_ne (T (4, -4), -4));
  ASSERT_TRUE (known_ne (T (4, -4), -3));

  /* Test known_ne (C, T).  */
  ASSERT_TRUE (known_ne (0, T (4, -3)));
  ASSERT_FALSE (known_ne (1, T (4, -3)));
  ASSERT_FALSE (known_ne (4, T (4, -3)));
  ASSERT_TRUE (known_ne (7, T (4, -3)));
  ASSERT_TRUE (known_ne (T (4, -3), -3));
  ASSERT_FALSE (known_ne (T (4, -3), -2));

  /* Test known_ne (T, T).  */
  ASSERT_FALSE (known_ne (T (0, 3), T (6, 1)));
  ASSERT_TRUE (known_ne (T (0, -3), T (6, 1)));
  ASSERT_TRUE (known_ne (T (0, 3), T (7, 1)));
  ASSERT_FALSE (known_ne (T (-3, 4), T (7, -1)));
  ASSERT_TRUE (known_ne (T (-3, 4), T (6, -1)));
}

/* Test negation for signed C, both via operators and wi::.  */

template<unsigned int N, typename C, typename RC, typename T>
static void
test_signed_negation ()
{
  typedef poly_helper<T> ph;
  typedef poly_helper< poly_int<N, RC> > rph;
  typedef poly_helper< poly_int<N, int> > iph;

  /* Test unary -.  */
  ASSERT_KNOWN_EQ (-ph::make (-11, 22, -33),
		   rph::make (11, -22, 33));

  /* Test wi::neg.  */
  ASSERT_KNOWN_EQ (wi::neg (ph::make (-11, 22, -33)),
		   iph::make (11, -22, 33));
}

/* Test maybe_le for signed C.  */

template<unsigned int N, typename C, typename T>
static void
test_signed_maybe_le ()
{
  typedef coeff_helper<C> ch;
  typedef poly_helper<T> ph;

  /* Test maybe_le (T, C).  */
  ASSERT_EQ (maybe_le (ph::make (3, 5, -1), ch::make (2)), N == 3);
  ASSERT_EQ (maybe_le (ph::make (40, -10, 60), ch::make (15)), N >= 2);
  ASSERT_TRUE (maybe_le (ph::make (-14, 0, 0), ch::make (13)));

  /* Test maybe_le (C, T).  */
  ASSERT_EQ (maybe_le (ch::make (4), ph::make (3, 5, -1)), N >= 2);
  ASSERT_EQ (maybe_le (ch::make (41), ph::make (40, -10, 60)), N == 3);
  ASSERT_TRUE (maybe_le (ch::make (-15), ph::make (11, 0, 0)));

  /* Test maybe_le (T, T).  */
  ASSERT_EQ (maybe_le (ph::make (-2, 4, -2),
		       ph::make (-3, -5, -1)), N == 3);
  ASSERT_EQ (maybe_le (ph::make (-2, -6, 0),
		       ph::make (-3, 0, 100)), N >= 2);
  ASSERT_FALSE (maybe_le (ph::make (-2, 5, 1),
			  ph::make (-3, 4, 0)));
}

/* Test maybe_lt for signed C.  */

template<unsigned int N, typename C, typename T>
static void
test_signed_maybe_lt ()
{
  typedef coeff_helper<C> ch;
  typedef poly_helper<T> ph;

  /* Test maybe_lt (T, C).  */
  ASSERT_EQ (maybe_lt (ph::make (3, 5, -1), ch::make (2)), N == 3);
  ASSERT_EQ (maybe_lt (ph::make (40, -10, 60), ch::make (15)), N >= 2);
  ASSERT_TRUE (maybe_lt (ph::make (-18, 0, 0), ch::make (18)));
  ASSERT_EQ (maybe_lt (ph::make (-2, -2, -2), ch::make (-2)), N >= 2);

  /* Test maybe_lt (C, T).  */
  ASSERT_EQ (maybe_lt (ch::make (4), ph::make (3, 5, -1)), N >= 2);
  ASSERT_EQ (maybe_lt (ch::make (41), ph::make (40, -10, 60)), N == 3);
  ASSERT_TRUE (maybe_lt (ch::make (-45), ph::make (40, 0, 0)));
  ASSERT_FALSE (maybe_lt (ch::make (-2), ph::make (-2, -2, -2)));

  /* Test maybe_lt (T, T).  */
  ASSERT_EQ (maybe_lt (ph::make (-3, 4, -2),
		       ph::make (-3, -5, -1)), N == 3);
  ASSERT_EQ (maybe_lt (ph::make (-3, -6, 0),
		       ph::make (-3, 0, 100)), N >= 2);
  ASSERT_FALSE (maybe_lt (ph::make (-3, 5, 1),
			  ph::make (-3, 4, 0)));
}

/* Test maybe_ge for signed C.  */

template<unsigned int N, typename C, typename T>
static void
test_signed_maybe_ge ()
{
  typedef coeff_helper<C> ch;
  typedef poly_helper<T> ph;

  /* Test maybe_ge (T, C).  */
  ASSERT_EQ (maybe_ge (ph::make (3, 5, -1), ch::make (4)), N >= 2);
  ASSERT_EQ (maybe_ge (ph::make (40, -10, 60), ch::make (41)), N == 3);
  ASSERT_TRUE (maybe_ge (ph::make (11, 0, 0), ch::make (-15)));

  /* Test maybe_ge (C, T).  */
  ASSERT_EQ (maybe_ge (ch::make (2), ph::make (3, 5, -1)), N == 3);
  ASSERT_EQ (maybe_ge (ch::make (15), ph::make (40, -10, 60)), N >= 2);
  ASSERT_TRUE (maybe_ge (ch::make (13), ph::make (-14, 0, 0)));

  /* Test maybe_ge (T, T).  */
  ASSERT_EQ (maybe_ge (ph::make (-3, -5, -1),
		       ph::make (-2, 4, -2)), N == 3);
  ASSERT_EQ (maybe_ge (ph::make (-3, 0, 100),
		       ph::make (-2, -6, 0)), N >= 2);
  ASSERT_FALSE (maybe_ge (ph::make (-3, 4, 0),
			  ph::make (-2, 5, 1)));
}

/* Test maybe_gt for signed C.  */

template<unsigned int N, typename C, typename T>
static void
test_signed_maybe_gt ()
{
  typedef coeff_helper<C> ch;
  typedef poly_helper<T> ph;

  /* Test maybe_gt (T, C).  */
  ASSERT_EQ (maybe_gt (ph::make (3, 5, -1), ch::make (4)), N >= 2);
  ASSERT_EQ (maybe_gt (ph::make (40, -10, 60), ch::make (41)), N == 3);
  ASSERT_TRUE (maybe_gt (ph::make (40, 0, 0), ch::make (-45)));
  ASSERT_FALSE (maybe_gt (ph::make (-2, -2, -2), ch::make (-2)));

  /* Test maybe_gt (C, T).  */
  ASSERT_EQ (maybe_gt (ch::make (2), ph::make (3, 5, -1)), N == 3);
  ASSERT_EQ (maybe_gt (ch::make (15), ph::make (40, -10, 60)), N >= 2);
  ASSERT_TRUE (maybe_gt (ch::make (18), ph::make (-18, 0, 0)));
  ASSERT_EQ (maybe_gt (ch::make (-2), ph::make (-2, -2, -2)), N >= 2);

  /* Test maybe_gt (T, T).  */
  ASSERT_EQ (maybe_gt (ph::make (-3, -5, -1),
		       ph::make (-3, 4, -2)), N == 3);
  ASSERT_EQ (maybe_gt (ph::make (-3, 0, 100),
		       ph::make (-3, -6, 0)), N >= 2);
  ASSERT_FALSE (maybe_gt (ph::make (-3, 4, 0),
			  ph::make (-3, 5, 1)));
}

/* Test known_gt for signed C.  */

template<unsigned int N, typename C, typename T>
static void
test_signed_known_gt ()
{
  typedef coeff_helper<C> ch;
  typedef poly_helper<T> ph;

  /* Test known_gt (T, C).  */
  ASSERT_EQ (known_gt (ph::make (3, 5, -1), ch::make (2)), N <= 2);
  ASSERT_EQ (known_gt (ph::make (40, -10, 60), ch::make (15)), N == 1);
  ASSERT_FALSE (known_gt (ph::make (-14, 0, 0), ch::make (13)));

  /* Test known_gt (C, T).  */
  ASSERT_EQ (known_gt (ch::make (4), ph::make (3, 5, -1)), N == 1);
  ASSERT_EQ (known_gt (ch::make (41), ph::make (40, -10, 60)), N <= 2);
  ASSERT_FALSE (known_gt (ch::make (-15), ph::make (11, 0, 0)));

  /* Test known_gt (T, T).  */
  ASSERT_EQ (known_gt (ph::make (-2, 4, -2),
		       ph::make (-3, -5, -1)), N <= 2);
  ASSERT_EQ (known_gt (ph::make (-2, -6, 0),
		       ph::make (-3, 0, 100)), N == 1);
  ASSERT_TRUE (known_gt (ph::make (-2, 5, 1),
			 ph::make (-3, 4, 0)));
}

/* Test known_ge for signed C.  */

template<unsigned int N, typename C, typename T>
static void
test_signed_known_ge ()
{
  typedef coeff_helper<C> ch;
  typedef poly_helper<T> ph;

  /* Test known_ge (T, C).  */
  ASSERT_EQ (known_ge (ph::make (3, 5, -1), ch::make (2)), N <= 2);
  ASSERT_EQ (known_ge (ph::make (40, -10, 60), ch::make (15)), N == 1);
  ASSERT_FALSE (known_ge (ph::make (-18, 0, 0), ch::make (18)));
  ASSERT_EQ (known_ge (ph::make (-2, -2, -2), ch::make (-2)), N == 1);

  /* Test known_ge (C, T).  */
  ASSERT_EQ (known_ge (ch::make (4), ph::make (3, 5, -1)), N == 1);
  ASSERT_EQ (known_ge (ch::make (41), ph::make (40, -10, 60)), N <= 2);
  ASSERT_FALSE (known_ge (ch::make (-45), ph::make (40, 0, 0)));
  ASSERT_TRUE (known_ge (ch::make (-2), ph::make (-2, -2, -2)));

  /* Test known_ge (T, T).  */
  ASSERT_EQ (known_ge (ph::make (-3, 4, -2),
		       ph::make (-3, -5, -1)), N <= 2);
  ASSERT_EQ (known_ge (ph::make (-3, -6, 0),
		       ph::make (-3, 0, 100)), N == 1);
  ASSERT_TRUE (known_ge (ph::make (-3, 5, 1),
			 ph::make (-3, 4, 0)));
}

/* Test known_lt for signed C.  */

template<unsigned int N, typename C, typename T>
static void
test_signed_known_lt ()
{
  typedef coeff_helper<C> ch;
  typedef poly_helper<T> ph;

  /* Test known_lt (T, C).  */
  ASSERT_EQ (known_lt (ph::make (3, 5, -1), ch::make (4)), N == 1);
  ASSERT_EQ (known_lt (ph::make (40, -10, 60), ch::make (41)), N <= 2);
  ASSERT_FALSE (known_lt (ph::make (11, 0, 0), ch::make (-15)));

  /* Test known_lt (C, T).  */
  ASSERT_EQ (known_lt (ch::make (2), ph::make (3, 5, -1)), N <= 2);
  ASSERT_EQ (known_lt (ch::make (15), ph::make (40, -10, 60)), N == 1);
  ASSERT_FALSE (known_lt (ch::make (13), ph::make (-14, 0, 0)));

  /* Test known_lt (T, T).  */
  ASSERT_EQ (known_lt (ph::make (-3, -5, -1),
		       ph::make (-2, 4, -2)), N <= 2);
  ASSERT_EQ (known_lt (ph::make (-3, 0, 100),
		       ph::make (-2, -6, 0)), N == 1);
  ASSERT_TRUE (known_lt (ph::make (-3, 4, 0),
			 ph::make (-2, 5, 1)));
}

/* Test known_le for signed C.  */

template<unsigned int N, typename C, typename T>
static void
test_signed_known_le ()
{
  typedef coeff_helper<C> ch;
  typedef poly_helper<T> ph;

  /* Test known_le (T, C).  */
  ASSERT_EQ (known_le (ph::make (3, 5, -1), ch::make (4)), N == 1);
  ASSERT_EQ (known_le (ph::make (40, -10, 60), ch::make (41)), N <= 2);
  ASSERT_FALSE (known_le (ph::make (40, 0, 0), ch::make (-45)));
  ASSERT_TRUE (known_le (ph::make (-2, -2, -2), ch::make (-2)));

  /* Test known_le (C, T).  */
  ASSERT_EQ (known_le (ch::make (2), ph::make (3, 5, -1)), N <= 2);
  ASSERT_EQ (known_le (ch::make (15), ph::make (40, -10, 60)), N == 1);
  ASSERT_FALSE (known_le (ch::make (18), ph::make (-18, 0, 0)));
  ASSERT_EQ (known_le (ch::make (-2), ph::make (-2, -2, -2)), N == 1);

  /* Test known_le (T, T).  */
  ASSERT_EQ (known_le (ph::make (-3, -5, -1),
		       ph::make (-3, 4, -2)), N <= 2);
  ASSERT_EQ (known_le (ph::make (-3, 0, 100),
		       ph::make (-3, -6, 0)), N == 1);
  ASSERT_TRUE (known_le (ph::make (-3, 4, 0),
			 ph::make (-3, 5, 1)));
}

/* Test ordered_p for signed C.  */

template<unsigned int N, typename C, typename T>
static void
test_signed_ordered_p ()
{
  typedef coeff_helper<C> ch;
  typedef poly_helper<T> ph;

  /* Test ordered_p (T, C).  */
  ASSERT_EQ (ordered_p (ph::make (3, 5, -1), ch::make (4)), N == 1);
  ASSERT_EQ (ordered_p (ph::make (3, 5, -1), ch::make (3)), N <= 2);
  ASSERT_EQ (ordered_p (ph::make (3, 5, -1), ch::make (2)), N <= 2);
  ASSERT_EQ (ordered_p (ph::make (40, -10, 60), ch::make (41)), N <= 2);
  ASSERT_EQ (ordered_p (ph::make (40, -10, 60), ch::make (40)), N <= 2);
  ASSERT_EQ (ordered_p (ph::make (40, -10, 60), ch::make (39)), N == 1);
  ASSERT_EQ (ordered_p (ph::make (4, -4, -4), ch::make (0)), N == 1);
  ASSERT_EQ (ordered_p (ph::make (4, 0, -4), ch::make (0)), N <= 2);
  ASSERT_EQ (ordered_p (ph::make (4, 4, -4), ch::make (0)), N <= 2);
  ASSERT_EQ (ordered_p (ph::make (-4, 4, 4), ch::make (0)), N == 1);
  ASSERT_EQ (ordered_p (ph::make (-4, 0, 4), ch::make (0)), N <= 2);
  ASSERT_EQ (ordered_p (ph::make (-4, -4, 4), ch::make (0)), N <= 2);

  /* Test ordered_p (C, T).  */
  ASSERT_EQ (ordered_p (ch::make (4), ph::make (3, 5, -1)), N == 1);
  ASSERT_EQ (ordered_p (ch::make (3), ph::make (3, 5, -1)), N <= 2);
  ASSERT_EQ (ordered_p (ch::make (2), ph::make (3, 5, -1)), N <= 2);
  ASSERT_EQ (ordered_p (ch::make (41), ph::make (40, -10, 60)), N <= 2);
  ASSERT_EQ (ordered_p (ch::make (40), ph::make (40, -10, 60)), N <= 2);
  ASSERT_EQ (ordered_p (ch::make (39), ph::make (40, -10, 60)), N == 1);
  ASSERT_EQ (ordered_p (ch::make (0), ph::make (4, -4, -4)), N == 1);
  ASSERT_EQ (ordered_p (ch::make (0), ph::make (4, 0, -4)), N <= 2);
  ASSERT_EQ (ordered_p (ch::make (0), ph::make (4, 4, -4)), N <= 2);
  ASSERT_EQ (ordered_p (ch::make (0), ph::make (-4, 4, 4)), N == 1);
  ASSERT_EQ (ordered_p (ch::make (0), ph::make (-4, 0, 4)), N <= 2);
  ASSERT_EQ (ordered_p (ch::make (0), ph::make (-4, -4, 4)), N <= 2);
}

/* Test ordered_min for signed C.  */

template<unsigned int N, typename C, typename T>
static void
test_signed_ordered_min ()
{
  typedef coeff_helper<C> ch;
  typedef poly_helper<T> ph;

  /* Test ordered_min (T, C).  */
  ASSERT_KNOWN_EQ (ordered_min (ph::make (4, -12, -14), ch::make (5)),
		   ph::make (4, -12, -14));

  /* Test ordered_min (C, T).  */
  ASSERT_KNOWN_EQ (ordered_min (ch::make (9), ph::make (9, -90, -77)),
		   ph::make (9, -90, -77));

  /* Test ordered_min (T, T).  */
  ASSERT_KNOWN_EQ (ordered_min (ph::make (4, 9, 17), ph::make (4, -1, 17)),
		   ph::make (4, -1, 17));
}

/* Test ordered_max for signed C.  */

template<unsigned int N, typename C, typename T>
static void
test_signed_ordered_max ()
{
  typedef coeff_helper<C> ch;
  typedef poly_helper<T> ph;

  /* Test ordered_max (T, C).  */
  ASSERT_KNOWN_EQ (ordered_max (ph::make (4, -12, -14), ch::make (5)),
		   ch::make (5));

  /* Test ordered_max (C, T).  */
  ASSERT_KNOWN_EQ (ordered_max (ch::make (9), ph::make (9, -90, -77)),
		   ch::make (9));

  /* Test ordered_max (T, T).  */
  ASSERT_KNOWN_EQ (ordered_max (ph::make (4, 9, 17), ph::make (4, -1, 17)),
		   ph::make (4, 9, 17));
}

/* Test lower_bound for signed C.  */

template<unsigned int N, typename C, typename T>
static void
test_signed_lower_bound ()
{
  typedef coeff_helper<C> ch;
  typedef poly_helper<T> ph;

  /* Test lower_bound (T, C).  */
  ASSERT_KNOWN_EQ (lower_bound (ph::make (4, -1, 3), ch::make (5)),
		   ph::make (4, -1, 0));
  ASSERT_KNOWN_EQ (lower_bound (ph::make (6, 5, -14), ch::make (-11)),
		   ph::make (-11, 0, -14));

  /* Test lower_bound (C, T).  */
  ASSERT_KNOWN_EQ (lower_bound (ch::make (5), ph::make (4, -1, 3)),
		   ph::make (4, -1, 0));
  ASSERT_KNOWN_EQ (lower_bound (ch::make (-11), ph::make (6, 5, -14)),
		   ph::make (-11, 0, -14));

  /* Test lower_bound (T, T).  */
  ASSERT_KNOWN_EQ (lower_bound (ph::make (4, -1, 3), ph::make (5, 7, -2)),
		   ph::make (4, -1, -2));
  ASSERT_KNOWN_EQ (lower_bound (ph::make (6, 5, -14), ph::make (-11, 4, 3)),
		   ph::make (-11, 4, -14));
}

/* Test upper_bound for signed C.  */

template<unsigned int N, typename C, typename T>
static void
test_signed_upper_bound ()
{
  typedef coeff_helper<C> ch;
  typedef poly_helper<T> ph;

  /* Test upper_bound (T, C).  */
  ASSERT_KNOWN_EQ (upper_bound (ph::make (4, -1, 3), ch::make (5)),
		   ph::make (5, 0, 3));
  ASSERT_KNOWN_EQ (upper_bound (ph::make (6, 5, -14), ch::make (-11)),
		   ph::make (6, 5, 0));

  /* Test upper_bound (C, T).  */
  ASSERT_KNOWN_EQ (upper_bound (ch::make (5), ph::make (4, -1, 3)),
		   ph::make (5, 0, 3));
  ASSERT_KNOWN_EQ (upper_bound (ch::make (-11), ph::make (6, 5, -14)),
		   ph::make (6, 5, 0));

  /* Test upper_bound (T, T).  */
  ASSERT_KNOWN_EQ (upper_bound (ph::make (4, -1, 3), ph::make (5, 7, -2)),
		   ph::make (5, 7, 3));
  ASSERT_KNOWN_EQ (upper_bound (ph::make (6, 5, -14), ph::make (-11, 4, 3)),
		   ph::make (6, 5, 3));
}

/* Test constant_multiple_p for signed C.  */

template<unsigned int N, typename C, typename T>
static void
test_signed_constant_multiple_p ()
{
  typedef poly_helper<T> ph;

  /* Test constant_multiple_p (T, C).  */
  C const_multiple;
  ASSERT_TRUE (constant_multiple_p (ph::make (-45, 0, 0), 9,
				    &const_multiple));
  ASSERT_EQ (const_multiple, -5);
  ASSERT_TRUE (constant_multiple_p (ph::make (63, 0, 0), -7,
				    &const_multiple));
  ASSERT_EQ (const_multiple, -9);
  ASSERT_FALSE (constant_multiple_p (ph::make (-121, 0, 0), -12,
				     &const_multiple));
  ASSERT_TRUE (constant_multiple_p (ph::make (-120, 0, 0), -12,
				    &const_multiple));
  ASSERT_EQ (const_multiple, 10);
  ASSERT_FALSE (constant_multiple_p (ph::make (-119, 0, 0), -12,
				     &const_multiple));
  ASSERT_EQ (constant_multiple_p (ph::make (-120, -23, 12), 12,
				  &const_multiple), N == 1);
  if (N == 1)
    ASSERT_EQ (const_multiple, -10);

  /* Test constant_multiple_p (C, T).  */
  ASSERT_TRUE (constant_multiple_p (-45, ph::make (9, 0, 0),
				    &const_multiple));
  ASSERT_EQ (const_multiple, -5);
  ASSERT_TRUE (constant_multiple_p (63, ph::make (-7, 0, 0),
				    &const_multiple));
  ASSERT_EQ (const_multiple, -9);
  ASSERT_FALSE (constant_multiple_p (-121, ph::make (-12, 0, 0),
				     &const_multiple));
  ASSERT_TRUE (constant_multiple_p (-120, ph::make (-12, 0, 0),
				    &const_multiple));
  ASSERT_EQ (const_multiple, 10);
  ASSERT_FALSE (constant_multiple_p (-119, ph::make (-12, 0, 0),
				     &const_multiple));
  ASSERT_EQ (constant_multiple_p (-120, ph::make (12, 10, 6),
				  &const_multiple), N == 1);
  if (N == 1)
    ASSERT_EQ (const_multiple, -10);

  ASSERT_TRUE (constant_multiple_p (ph::make (-40, 80, -200),
				    ph::make (2, -4, 10),
				    &const_multiple));
  ASSERT_EQ (const_multiple, -20);
  ASSERT_EQ (constant_multiple_p (ph::make (-20, 40, 100),
				  ph::make (2, -4, 10),
				  &const_multiple), N <= 2);
  ASSERT_EQ (const_multiple, N <= 2 ? -10 : -20);
  ASSERT_EQ (constant_multiple_p (ph::make (-10, -20, -50),
				  ph::make (2, -4, 10),
				  &const_multiple), N == 1);
  ASSERT_EQ (const_multiple, N == 1 ? -5 : N == 2 ? -10 : -20);
  ASSERT_FALSE (constant_multiple_p (ph::make (-31, 0, 0),
				     ph::make (-6, 0, 0),
				     &const_multiple));
  ASSERT_TRUE (constant_multiple_p (ph::make (-30, 0, 0),
				    ph::make (-6, 0, 0),
				    &const_multiple));
  ASSERT_EQ (const_multiple, 5);
  ASSERT_FALSE (constant_multiple_p (ph::make (-29, 0, 0),
				     ph::make (-6, 0, 0),
				     &const_multiple));
}

/* Test multiple_p for signed C.  */

template<unsigned int N, typename C, typename T>
static void
test_signed_multiple_p ()
{
  typedef poly_helper<T> ph;

  /* Test multiple_p (T, C).  */
  ASSERT_TRUE (multiple_p (ph::make (-45, 36, 0), 9));
  ASSERT_TRUE (multiple_p (ph::make (63, 0, -14), -7));
  ASSERT_FALSE (multiple_p (ph::make (-121, 0, 0), -12));
  ASSERT_TRUE (multiple_p (ph::make (-120, 0, 0), -12));
  ASSERT_FALSE (multiple_p (ph::make (-119, 0, 0), -12));
  ASSERT_TRUE (multiple_p (ph::make (-120, -24, 12), 12));
  ASSERT_EQ (multiple_p (ph::make (-120, -24, 11), 12), N <= 2);
  ASSERT_EQ (multiple_p (ph::make (-120, -23, 12), 12), N == 1);

  /* Test multiple_p (C, T).  */
  ASSERT_TRUE (multiple_p (-45, ph::make (9, 0, 0)));
  ASSERT_TRUE (multiple_p (63, ph::make (-7, 0, 0)));
  ASSERT_FALSE (multiple_p (-121, ph::make (-12, 0, 0)));
  ASSERT_TRUE (multiple_p (-120, ph::make (-12, 0, 0)));
  ASSERT_FALSE (multiple_p (-119, ph::make (-12, 0, 0)));
  ASSERT_EQ (multiple_p (-120, ph::make (12, 10, 6)), N == 1);

  /* Test multiple_p (T, T).  */
  ASSERT_TRUE (multiple_p (ph::make (-40, 80, -200),
			   ph::make (2, -4, 10)));
  ASSERT_EQ (multiple_p (ph::make (-20, 40, 100),
			 ph::make (2, -4, 10)), N <= 2);
  ASSERT_EQ (multiple_p (ph::make (-10, -20, -50),
			 ph::make (2, -4, 10)), N == 1);
  ASSERT_FALSE (multiple_p (ph::make (-31, 0, 0),
			    ph::make (-6, 0, 0)));
  ASSERT_TRUE (multiple_p (ph::make (-30, 0, 0),
			   ph::make (-6, 0, 0)));
  ASSERT_FALSE (multiple_p (ph::make (-29, 0, 0),
			    ph::make (-6, 0, 0)));
}

/* Test the 3-operand form of multiple_p for signed C.  */

template<unsigned int N, typename C, typename T>
static void
test_signed_multiple_p_with_result ()
{
  typedef coeff_helper<C> ch;
  typedef poly_helper<T> ph;

  /* Test multiple_p (T, C) -> T.  */
  T multiple;
  ASSERT_TRUE (multiple_p (ph::make (-45, 36, 0), 9, &multiple));
  ASSERT_KNOWN_EQ (multiple, ph::make (-5, 4, 0));
  ASSERT_TRUE (multiple_p (ph::make (63, 0, -14), -7, &multiple));
  ASSERT_KNOWN_EQ (multiple, ph::make (-9, 0, 2));
  ASSERT_FALSE (multiple_p (ph::make (-121, 0, 0), -12, &multiple));
  ASSERT_TRUE (multiple_p (ph::make (-120, 0, 0), -12, &multiple));
  ASSERT_KNOWN_EQ (multiple, ch::make (10));
  ASSERT_FALSE (multiple_p (ph::make (-119, 0, 0), -12, &multiple));
  ASSERT_TRUE (multiple_p (ph::make (-120, -24, 12), 12, &multiple));
  ASSERT_KNOWN_EQ (multiple, ph::make (-10, -2, 1));
  ASSERT_EQ (multiple_p (ph::make (-120, -24, 11), 12, &multiple), N <= 2);
  if (N <= 2)
    ASSERT_KNOWN_EQ (multiple, ph::make (-10, -2, 0));
  ASSERT_EQ (multiple_p (ph::make (-120, -23, 12), 12, &multiple), N == 1);
  if (N == 1)
    ASSERT_KNOWN_EQ (multiple, ch::make (-10));

  /* Test multiple_p (C, T) -> T.  */
  ASSERT_TRUE (multiple_p (-45, ph::make (9, 0, 0), &multiple));
  ASSERT_KNOWN_EQ (multiple, ch::make (-5));
  ASSERT_TRUE (multiple_p (63, ph::make (-7, 0, 0), &multiple));
  ASSERT_KNOWN_EQ (multiple, ch::make (-9));
  ASSERT_FALSE (multiple_p (-121, ph::make (-12, 0, 0), &multiple));
  ASSERT_TRUE (multiple_p (-120, ph::make (-12, 0, 0), &multiple));
  ASSERT_KNOWN_EQ (multiple, ch::make (10));
  ASSERT_FALSE (multiple_p (-119, ph::make (-12, 0, 0), &multiple));
  ASSERT_EQ (multiple_p (-120, ph::make (12, 10, 6), &multiple), N == 1);
  ASSERT_KNOWN_EQ (multiple, ch::make (N == 1 ? -10 : 10));

  /* Test multiple_p (T, T) -> T.  */
  ASSERT_TRUE (multiple_p (ph::make (-40, 80, -200),
			   ph::make (2, -4, 10),
			   &multiple));
  ASSERT_KNOWN_EQ (multiple, ch::make (-20));
  ASSERT_EQ (multiple_p (ph::make (-20, 40, 100),
			 ph::make (2, -4, 10),
			 &multiple), N <= 2);
  if (N <= 2)
    ASSERT_KNOWN_EQ (multiple, ch::make (-10));
  ASSERT_EQ (multiple_p (ph::make (-10, -20, -50),
			 ph::make (2, -4, 10),
			 &multiple), N == 1);
  if (N == 1)
    ASSERT_KNOWN_EQ (multiple, ch::make (-5));
  ASSERT_FALSE (multiple_p (ph::make (-31, 0, 0),
			    ph::make (-6, 0, 0),
			    &multiple));
  ASSERT_TRUE (multiple_p (ph::make (-30, 0, 0),
			   ph::make (-6, 0, 0),
			   &multiple));
  ASSERT_KNOWN_EQ (multiple, ch::make (5));
  ASSERT_FALSE (multiple_p (ph::make (-29, 0, 0),
			    ph::make (-6, 0, 0),
			    &multiple));
}

/* Test exact_div for signed C.  */

template<unsigned int N, typename C, typename T>
static void
test_signed_exact_div ()
{
  typedef coeff_helper<C> ch;
  typedef poly_helper<T> ph;

  /* Test exact_div (T, C).  */
  ASSERT_KNOWN_EQ (exact_div (ph::make (-45, 36, 0), 9),
		   ph::make (-5, 4, 0));
  ASSERT_KNOWN_EQ (exact_div (ph::make (63, 0, -14), -7),
		   ph::make (-9, 0, 2));
  ASSERT_KNOWN_EQ (exact_div (ph::make (-120, 0, 0), -12),
		   ch::make (10));
  ASSERT_KNOWN_EQ (exact_div (ph::make (-120, -24, 12), 12),
		   ph::make (-10, -2, 1));

  /* Test exact_div (T, T).  */
  ASSERT_KNOWN_EQ (exact_div (ph::make (-40, 80, -200),
			      ph::make (2, -4, 10)),
		   ch::make (-20));
  ASSERT_KNOWN_EQ (exact_div (ph::make (-30, 0, 0),
			      ph::make (-6, 0, 0)),
		   ch::make (5));
}

/* Test the form of can_div_trunc_p that returns a constant, for signed C.  */

template<unsigned int N, typename C, typename T>
static void
test_signed_can_div_trunc_p_const ()
{
  typedef coeff_helper<C> ch;
  typedef poly_helper<T> ph;

  /* Test can_div_trunc_p (T, C) -> C.  */
  C const_quot;
  ASSERT_TRUE (can_div_trunc_p (ph::make (-31, 0, 0), 10, &const_quot));
  ASSERT_KNOWN_EQ (const_quot, -3);
  ASSERT_TRUE (can_div_trunc_p (ph::make (-29, 0, 0), 10, &const_quot));
  ASSERT_KNOWN_EQ (const_quot, -2);

  /* Test can_div_trunc_p (T, T) -> C.  */
  ASSERT_TRUE (can_div_trunc_p (ph::make (-10, 25, -15),
				ph::make (2, -5, 3),
				&const_quot));
  ASSERT_EQ (const_quot, -5);
  /* (-5 + 2x) / (-3 + 2x) != 1 when x == 1.  */
  ASSERT_EQ (can_div_trunc_p (ph::make (-5, 2, 0),
			      ph::make (-3, 2, 0),
			      &const_quot), N == 1);
  ASSERT_EQ (const_quot, N == 1 ? 1 : -5);
  /* Similarly for the third coefficient.  */
  ASSERT_EQ (can_div_trunc_p (ph::make (-5, -5, 2),
			      ph::make (-3, -3, 2),
			      &const_quot), N <= 2);
  ASSERT_EQ (const_quot, N <= 2 ? 1 : -5);
  /* (-15 + 3x) / (-12 + 2x) != 1 when x == 7.  */
  ASSERT_EQ (can_div_trunc_p (ph::make (-15, 3, 0),
			      ph::make (-12, 2, 0),
			      &const_quot), N == 1);
  ASSERT_EQ (const_quot, N <= 2 ? 1 : -5);
  ASSERT_TRUE (can_div_trunc_p (ph::make (-21, -18, -14),
				ph::make (5, 4, 3),
				&const_quot));
  ASSERT_EQ (const_quot, -4);
  ASSERT_TRUE (can_div_trunc_p (ph::make (18, 9, 13),
				ph::make (-8, -4, -5),
				&const_quot));
  ASSERT_EQ (const_quot, -2);

  /* Test can_div_trunc_p (T, T) -> C, T.  */
  T rem;
  ASSERT_TRUE (can_div_trunc_p (ph::make (-10, 25, -15),
				ph::make (2, -5, 3),
				&const_quot, &rem));
  ASSERT_EQ (const_quot, -5);
  ASSERT_KNOWN_EQ (rem, ch::make (0));
  /* (-5 + 2x) / (-3 + 2x) != 1 when x == 1.  */
  ASSERT_EQ (can_div_trunc_p (ph::make (-5, 2, 0),
			      ph::make (-3, 2, 0),
			      &const_quot, &rem), N == 1);
  ASSERT_KNOWN_EQ (const_quot, N == 1 ? 1 : -5);
  if (N == 1)
    ASSERT_KNOWN_EQ (rem, ch::make (-2));
  /* Similarly for the third coefficient.  */
  ASSERT_EQ (can_div_trunc_p (ph::make (-5, -5, 2),
			      ph::make (-3, -3, 2),
			      &const_quot, &rem), N <= 2);
  ASSERT_KNOWN_EQ (const_quot, N <= 2 ? 1 : -5);
  if (N <= 2)
    ASSERT_KNOWN_EQ (rem, ph::make (-2, -2, 0));
  /* (-15 + 3x) / (-12 + 2x) != 1 when x == 7.  */
  ASSERT_EQ (can_div_trunc_p (ph::make (-15, 3, 0),
			      ph::make (-12, 2, 0),
			      &const_quot, &rem), N == 1);
  ASSERT_KNOWN_EQ (const_quot, N <= 2 ? 1 : -5);
  if (N == 1)
    ASSERT_KNOWN_EQ (rem, ch::make (-3));
  ASSERT_TRUE (can_div_trunc_p (ph::make (-21, -18, -14),
				ph::make (5, 4, 3),
				&const_quot, &rem));
  ASSERT_EQ (const_quot, -4);
  ASSERT_KNOWN_EQ (rem, ph::make (-1, -2, -2));
  ASSERT_TRUE (can_div_trunc_p (ph::make (18, 9, 13),
				ph::make (-8, -4, -5),
				&const_quot, &rem));
  ASSERT_EQ (const_quot, -2);
  ASSERT_KNOWN_EQ (rem, ph::make (2, 1, 3));
}

/* Test the form of can_div_trunc_p that returns a poly_int, for signed C.  */

template<unsigned int N, typename C, typename T>
static void
test_signed_can_div_trunc_p_poly ()
{
  typedef coeff_helper<C> ch;
  typedef poly_helper<T> ph;

  /* Test can_div_trunc_p (T, C) -> T.  */
  T quot;
  ASSERT_TRUE (can_div_trunc_p (ph::make (-99, 0, 0), 10, &quot));
  ASSERT_KNOWN_EQ (quot, ch::make (-9));
  ASSERT_TRUE (can_div_trunc_p (ph::make (7, -63, 81), 9, &quot));
  ASSERT_KNOWN_EQ (quot, ph::make (0, -7, 9));
  ASSERT_TRUE (can_div_trunc_p (ph::make (15, 44, -55), -11, &quot));
  ASSERT_KNOWN_EQ (quot, ph::make (-1, -4, 5));
  ASSERT_EQ (can_div_trunc_p (ph::make (-63, -24, -17), -8, &quot), N <= 2);
  if (N <= 2)
    ASSERT_KNOWN_EQ (quot, ph::make (7, 3, 0));
  ASSERT_EQ (can_div_trunc_p (ph::make (40, 48, 70), -7, &quot), N == 1);
  if (N == 1)
    ASSERT_KNOWN_EQ (quot, ch::make (-5));

  /* Test can_div_trunc_p (T, C) -> T, C.  */
  C const_rem;
  ASSERT_TRUE (can_div_trunc_p (ph::make (-99, 0, 0), 10,
				&quot, &const_rem));
  ASSERT_KNOWN_EQ (quot, ch::make (-9));
  ASSERT_EQ (const_rem, -9);
  ASSERT_TRUE (can_div_trunc_p (ph::make (7, -63, 81), 9,
				&quot, &const_rem));
  ASSERT_KNOWN_EQ (quot, ph::make (0, -7, 9));
  ASSERT_EQ (const_rem, 7);
  ASSERT_TRUE (can_div_trunc_p (ph::make (15, 44, -55), -11,
				&quot, &const_rem));
  ASSERT_KNOWN_EQ (quot, ph::make (-1, -4, 5));
  ASSERT_EQ (const_rem, 4);
  ASSERT_EQ (can_div_trunc_p (ph::make (-63, -24, -17), -8,
			      &quot, &const_rem), N <= 2);
  if (N <= 2)
    {
      ASSERT_KNOWN_EQ (quot, ph::make (7, 3, 0));
      ASSERT_EQ (const_rem, -7);
    }
  ASSERT_EQ (can_div_trunc_p (ph::make (40, 48, 70), -7,
			      &quot, &const_rem), N == 1);
  if (N == 1)
    {
      ASSERT_KNOWN_EQ (quot, ch::make (-5));
      ASSERT_EQ (const_rem, 5);
    }
}

/* Test can_div_away_from_zero_p for signed C.  */

template<unsigned int N, typename C, typename T>
static void
test_signed_can_div_away_from_zero_p ()
{
  typedef poly_helper<T> ph;

  /* Test can_div_away_from_zero_p (T, T) -> C.  */
  C const_quot;
  ASSERT_TRUE (can_div_away_from_zero_p (ph::make (-10, 25, -15),
					 ph::make (2, -5, 3),
					 &const_quot));
  ASSERT_EQ (const_quot, -5);
  /* (-5 + 2x) / (-3 + 2x) != 1 when x == 1.  */
  ASSERT_EQ (can_div_away_from_zero_p (ph::make (-5, 2, 0),
				       ph::make (-3, 2, 0),
				       &const_quot), N == 1);
  ASSERT_EQ (const_quot, N == 1 ? 2 : -5);
  /* Similarly for the third coefficient.  */
  ASSERT_EQ (can_div_away_from_zero_p (ph::make (-5, -5, 2),
				       ph::make (-3, -3, 2),
				       &const_quot), N <= 2);
  ASSERT_EQ (const_quot, N <= 2 ? 2 : -5);
  /* (-15 + 3x) / (-12 + 2x) != 1 when x == 7.  */
  ASSERT_EQ (can_div_away_from_zero_p (ph::make (-15, 3, 0),
				       ph::make (-12, 2, 0),
				       &const_quot), N == 1);
  ASSERT_EQ (const_quot, N <= 2 ? 2 : -5);
  ASSERT_TRUE (can_div_away_from_zero_p (ph::make (-21, -18, -14),
					 ph::make (5, 4, 3),
					 &const_quot));
  ASSERT_EQ (const_quot, -5);
  ASSERT_TRUE (can_div_away_from_zero_p (ph::make (18, 9, 13),
					 ph::make (-8, -4, -5),
					 &const_quot));
  ASSERT_EQ (const_quot, -3);
}

/* Test maybe_in_range_p for signed C.  */

template<unsigned int N, typename C, typename T>
static void
test_signed_maybe_in_range_p ()
{
  typedef coeff_helper<C> ch;
  typedef poly_helper<T> ph;

  ASSERT_EQ (maybe_in_range_p (ch::make (4),
			       ph::make (5, 1, -2),
			       ph::make (-1, -1, -1)), N == 3);
  ASSERT_EQ (maybe_in_range_p (ch::make (4),
			       ph::make (5, -1, 2),
			       ph::make (-1, -1, -1)), N >= 2);
}

/* Test maybe_le for unsigned C.  */

template<unsigned int N, typename C, typename T>
static void
test_unsigned_maybe_le ()
{
  typedef coeff_helper<C> ch;
  typedef poly_helper<T> ph;

  /* Test maybe_le (T, C).  */
  ASSERT_FALSE (maybe_le (ph::make (3, 5, -1), ch::make (2)));
  ASSERT_FALSE (maybe_le (ph::make (40, -10, 60), ch::make (15)));
  ASSERT_FALSE (maybe_le (ph::make (-14, 0, 0), ch::make (13)));

  /* Test maybe_le (C, T).  */
  ASSERT_EQ (maybe_le (ch::make (4), ph::make (3, 5, -1)), N >= 2);
  ASSERT_EQ (maybe_le (ch::make (41), ph::make (40, -10, 60)), N >= 2);
  ASSERT_FALSE (maybe_le (ch::make (-15), ph::make (11, 0, 0)));

  /* Test maybe_le (T, T).  */
  ASSERT_EQ (maybe_le (ph::make (-2, 4, -2),
		       ph::make (-3, -5, -1)), N >= 2);
  ASSERT_EQ (maybe_le (ph::make (-2, -6, 0),
		       ph::make (-3, 0, 100)), N == 3);
  ASSERT_FALSE (maybe_le (ph::make (-2, 5, 1),
			  ph::make (-3, 4, 0)));
}

/* Test maybe_lt for unsigned C.  */

template<unsigned int N, typename C, typename T>
static void
test_unsigned_maybe_lt ()
{
  typedef coeff_helper<C> ch;
  typedef poly_helper<T> ph;

  /* Test maybe_lt (T, C).  */
  ASSERT_FALSE (maybe_lt (ph::make (3, 5, -1), ch::make (2)));
  ASSERT_FALSE (maybe_lt (ph::make (40, -10, 60), ch::make (15)));
  ASSERT_FALSE (maybe_lt (ph::make (-18, 0, 0), ch::make (18)));
  ASSERT_FALSE (maybe_lt (ph::make (-2, -2, -2), ch::make (-2)));

  /* Test maybe_lt (C, T).  */
  ASSERT_EQ (maybe_lt (ch::make (4), ph::make (3, 5, -1)), N >= 2);
  ASSERT_EQ (maybe_lt (ch::make (41), ph::make (40, -10, 60)), N >= 2);
  ASSERT_FALSE (maybe_lt (ch::make (-45), ph::make (40, 0, 0)));
  ASSERT_EQ (maybe_lt (ch::make (-2), ph::make (-2, -2, -2)), N >= 2);

  /* Test maybe_lt (T, T).  */
  ASSERT_EQ (maybe_lt (ph::make (-3, 4, -2),
		       ph::make (-3, -5, -1)), N >= 2);
  ASSERT_EQ (maybe_lt (ph::make (-3, -6, 0),
		       ph::make (-3, 0, 100)), N == 3);
  ASSERT_FALSE (maybe_lt (ph::make (-3, 5, 1),
			  ph::make (-3, 4, 0)));
}

/* Test maybe_ge for unsigned C.  */

template<unsigned int N, typename C, typename T>
static void
test_unsigned_maybe_ge ()
{
  typedef coeff_helper<C> ch;
  typedef poly_helper<T> ph;

  /* Test maybe_ge (T, C).  */
  ASSERT_EQ (maybe_ge (ph::make (3, 5, -1), ch::make (4)), N >= 2);
  ASSERT_EQ (maybe_ge (ph::make (40, -10, 60), ch::make (41)), N >= 2);
  ASSERT_FALSE (maybe_ge (ph::make (11, 0, 0), ch::make (-15)));

  /* Test maybe_ge (C, T).  */
  ASSERT_FALSE (maybe_ge (ch::make (2), ph::make (3, 5, -1)));
  ASSERT_FALSE (maybe_ge (ch::make (15), ph::make (40, -10, 60)));
  ASSERT_FALSE (maybe_ge (ch::make (13), ph::make (-14, 0, 0)));

  /* Test maybe_ge (T, T).  */
  ASSERT_EQ (maybe_ge (ph::make (-3, -5, -1),
		       ph::make (-2, 4, -2)), N >= 2);
  ASSERT_EQ (maybe_ge (ph::make (-3, 0, 100),
		       ph::make (-2, -6, 0)), N == 3);
  ASSERT_FALSE (maybe_ge (ph::make (-3, 4, 0),
			  ph::make (-2, 5, 1)));
}

/* Test maybe_gt for unsigned C.  */

template<unsigned int N, typename C, typename T>
static void
test_unsigned_maybe_gt ()
{
  typedef coeff_helper<C> ch;
  typedef poly_helper<T> ph;

  /* Test maybe_gt (T, C).  */
  ASSERT_EQ (maybe_gt (ph::make (3, 5, -1), ch::make (4)), N >= 2);
  ASSERT_EQ (maybe_gt (ph::make (40, -10, 60), ch::make (41)), N >= 2);
  ASSERT_FALSE (maybe_gt (ph::make (40, 0, 0), ch::make (-45)));
  ASSERT_EQ (maybe_gt (ph::make (-2, -2, -2), ch::make (-2)), N >= 2);

  /* Test maybe_gt (C, T).  */
  ASSERT_FALSE (maybe_gt (ch::make (2), ph::make (3, 5, -1)));
  ASSERT_FALSE (maybe_gt (ch::make (15), ph::make (40, -10, 60)));
  ASSERT_FALSE (maybe_gt (ch::make (18), ph::make (-18, 0, 0)));
  ASSERT_FALSE (maybe_gt (ch::make (-2), ph::make (-2, -2, -2)));

  /* Test maybe_gt (T, T).  */
  ASSERT_EQ (maybe_gt (ph::make (-3, -5, -1),
		       ph::make (-3, 4, -2)), N >= 2);
  ASSERT_EQ (maybe_gt (ph::make (-3, 0, 100),
		       ph::make (-3, -6, 0)), N == 3);
  ASSERT_FALSE (maybe_gt (ph::make (-3, 4, 0),
			  ph::make (-3, 5, 1)));
}

/* Test known_gt for unsigned C.  */

template<unsigned int N, typename C, typename T>
static void
test_unsigned_known_gt ()
{
  typedef coeff_helper<C> ch;
  typedef poly_helper<T> ph;

  /* Test known_gt (T, C).  */
  ASSERT_TRUE (known_gt (ph::make (3, 5, -1), ch::make (2)));
  ASSERT_TRUE (known_gt (ph::make (40, -10, 60), ch::make (15)));
  ASSERT_TRUE (known_gt (ph::make (-14, 0, 0), ch::make (13)));

  /* Test known_gt (C, T).  */
  ASSERT_EQ (known_gt (ch::make (4), ph::make (3, 5, -1)), N == 1);
  ASSERT_EQ (known_gt (ch::make (41), ph::make (40, -10, 60)), N == 1);
  ASSERT_TRUE (known_gt (ch::make (-15), ph::make (11, 0, 0)));

  /* Test known_gt (T, T).  */
  ASSERT_EQ (known_gt (ph::make (-2, 4, -2),
		       ph::make (-3, -5, -1)), N == 1);
  ASSERT_EQ (known_gt (ph::make (-2, -6, 0),
		       ph::make (-3, 0, 100)), N <= 2);
  ASSERT_TRUE (known_gt (ph::make (-2, 5, 1),
			 ph::make (-3, 4, 0)));
}

/* Test known_ge for unsigned C.  */

template<unsigned int N, typename C, typename T>
static void
test_unsigned_known_ge ()
{
  typedef coeff_helper<C> ch;
  typedef poly_helper<T> ph;

  /* Test known_ge (T, C).  */
  ASSERT_TRUE (known_ge (ph::make (3, 5, -1), ch::make (2)));
  ASSERT_TRUE (known_ge (ph::make (40, -10, 60), ch::make (15)));
  ASSERT_TRUE (known_ge (ph::make (-18, 0, 0), ch::make (18)));
  ASSERT_TRUE (known_ge (ph::make (-2, -2, -2), ch::make (-2)));

  /* Test known_ge (C, T).  */
  ASSERT_EQ (known_ge (ch::make (4), ph::make (3, 5, -1)), N == 1);
  ASSERT_EQ (known_ge (ch::make (41), ph::make (40, -10, 60)), N == 1);
  ASSERT_TRUE (known_ge (ch::make (-45), ph::make (40, 0, 0)));
  ASSERT_EQ (known_ge (ch::make (-2), ph::make (-2, -2, -2)), N == 1);

  /* Test known_ge (T, T).  */
  ASSERT_EQ (known_ge (ph::make (-3, 4, -2),
		       ph::make (-3, -5, -1)), N == 1);
  ASSERT_EQ (known_ge (ph::make (-3, -6, 0),
		       ph::make (-3, 0, 100)), N <= 2);
  ASSERT_TRUE (known_ge (ph::make (-3, 5, 1),
			 ph::make (-3, 4, 0)));
}

/* Test known_lt for unsigned C.  */

template<unsigned int N, typename C, typename T>
static void
test_unsigned_known_lt ()
{
  typedef coeff_helper<C> ch;
  typedef poly_helper<T> ph;

  /* Test known_lt (T, C).  */
  ASSERT_EQ (known_lt (ph::make (3, 5, -1), ch::make (4)), N == 1);
  ASSERT_EQ (known_lt (ph::make (40, -10, 60), ch::make (41)), N == 1);
  ASSERT_TRUE (known_lt (ph::make (11, 0, 0), ch::make (-15)));

  /* Test known_lt (C, T).  */
  ASSERT_TRUE (known_lt (ch::make (2), ph::make (3, 5, -1)));
  ASSERT_TRUE (known_lt (ch::make (15), ph::make (40, -10, 60)));
  ASSERT_TRUE (known_lt (ch::make (13), ph::make (-14, 0, 0)));

  /* Test known_lt (T, T).  */
  ASSERT_EQ (known_lt (ph::make (-3, -5, -1),
		       ph::make (-2, 4, -2)), N == 1);
  ASSERT_EQ (known_lt (ph::make (-3, 0, 100),
		       ph::make (-2, -6, 0)), N <= 2);
  ASSERT_TRUE (known_lt (ph::make (-3, 4, 0),
			 ph::make (-2, 5, 1)));
}

/* Test known_le for unsigned C.  */

template<unsigned int N, typename C, typename T>
static void
test_unsigned_known_le ()
{
  typedef coeff_helper<C> ch;
  typedef poly_helper<T> ph;

  /* Test known_le (T, C).  */
  ASSERT_EQ (known_le (ph::make (3, 5, -1), ch::make (4)), N == 1);
  ASSERT_EQ (known_le (ph::make (40, -10, 60), ch::make (41)), N == 1);
  ASSERT_TRUE (known_le (ph::make (40, 0, 0), ch::make (-45)));
  ASSERT_EQ (known_le (ph::make (-2, -2, -2), ch::make (-2)), N == 1);

  /* Test known_le (C, T).  */
  ASSERT_TRUE (known_le (ch::make (2), ph::make (3, 5, -1)));
  ASSERT_TRUE (known_le (ch::make (15), ph::make (40, -10, 60)));
  ASSERT_TRUE (known_le (ch::make (18), ph::make (-18, 0, 0)));
  ASSERT_TRUE (known_le (ch::make (-2), ph::make (-2, -2, -2)));

  /* Test known_le (T, T).  */
  ASSERT_EQ (known_le (ph::make (-3, -5, -1),
		       ph::make (-3, 4, -2)), N == 1);
  ASSERT_EQ (known_le (ph::make (-3, 0, 100),
		       ph::make (-3, -6, 0)), N <= 2);
  ASSERT_TRUE (known_le (ph::make (-3, 4, 0),
			 ph::make (-3, 5, 1)));
}

/* Test ordered_p for unsigned C.  */

template<unsigned int N, typename C, typename T>
static void
test_unsigned_ordered_p ()
{
  typedef coeff_helper<C> ch;
  typedef poly_helper<T> ph;

  /* Test ordered_p (T, C).  */
  ASSERT_EQ (ordered_p (ph::make (3, 5, -1), ch::make (4)), N == 1);
  ASSERT_TRUE (ordered_p (ph::make (3, 5, -1), ch::make (3)));
  ASSERT_TRUE (ordered_p (ph::make (3, 5, -1), ch::make (2)));
  ASSERT_EQ (ordered_p (ph::make (40, -10, 60), ch::make (41)), N == 1);
  ASSERT_TRUE (ordered_p (ph::make (40, -10, 60), ch::make (40)));
  ASSERT_TRUE (ordered_p (ph::make (40, -10, 60), ch::make (39)));
  ASSERT_TRUE (ordered_p (ph::make (4, -4, -4), ch::make (0)));
  ASSERT_TRUE (ordered_p (ph::make (4, 0, -4), ch::make (0)));
  ASSERT_TRUE (ordered_p (ph::make (4, 4, -4), ch::make (0)));
  ASSERT_TRUE (ordered_p (ph::make (-4, 4, 4), ch::make (0)));
  ASSERT_TRUE (ordered_p (ph::make (-4, 0, 4), ch::make (0)));
  ASSERT_TRUE (ordered_p (ph::make (-4, -4, 4), ch::make (0)));

  /* Test ordered_p (C, T).  */
  ASSERT_EQ (ordered_p (ch::make (4), ph::make (3, 5, -1)), N == 1);
  ASSERT_TRUE (ordered_p (ch::make (3), ph::make (3, 5, -1)));
  ASSERT_TRUE (ordered_p (ch::make (2), ph::make (3, 5, -1)));
  ASSERT_EQ (ordered_p (ch::make (41), ph::make (40, -10, 60)), N == 1);
  ASSERT_TRUE (ordered_p (ch::make (40), ph::make (40, -10, 60)));
  ASSERT_TRUE (ordered_p (ch::make (39), ph::make (40, -10, 60)));
  ASSERT_TRUE (ordered_p (ch::make (0), ph::make (4, -4, -4)));
  ASSERT_TRUE (ordered_p (ch::make (0), ph::make (4, 0, -4)));
  ASSERT_TRUE (ordered_p (ch::make (0), ph::make (4, 4, -4)));
  ASSERT_TRUE (ordered_p (ch::make (0), ph::make (-4, 4, 4)));
  ASSERT_TRUE (ordered_p (ch::make (0), ph::make (-4, 0, 4)));
  ASSERT_TRUE (ordered_p (ch::make (0), ph::make (-4, -4, 4)));
}

/* Test ordered_min for unsigned C.  */

template<unsigned int N, typename C, typename T>
static void
test_unsigned_ordered_min ()
{
  typedef coeff_helper<C> ch;
  typedef poly_helper<T> ph;

  /* Test ordered_min (T, C).  */
  ASSERT_KNOWN_EQ (ordered_min (ph::make (5, -12, -14), ch::make (5)),
		   ch::make (5));

  /* Test ordered_min (C, T).  */
  ASSERT_KNOWN_EQ (ordered_min (ch::make (9), ph::make (9, -90, -77)),
		   ch::make (9));

  /* Test ordered_min (T, T).  */
  ASSERT_KNOWN_EQ (ordered_min (ph::make (4, 9, 17), ph::make (4, -1, 17)),
		   ph::make (4, 9, 17));
}

/* Test ordered_max for unsigned C.  */

template<unsigned int N, typename C, typename T>
static void
test_unsigned_ordered_max ()
{
  typedef coeff_helper<C> ch;
  typedef poly_helper<T> ph;

  /* Test ordered_max (T, C).  */
  ASSERT_KNOWN_EQ (ordered_max (ph::make (5, -12, -14), ch::make (5)),
		   ph::make (5, -12, -14));

  /* Test ordered_max (C, T).  */
  ASSERT_KNOWN_EQ (ordered_max (ch::make (9), ph::make (9, -90, -77)),
		   ph::make (9, -90, -77));

  /* Test ordered_max (T, T).  */
  ASSERT_KNOWN_EQ (ordered_max (ph::make (4, 9, 17), ph::make (4, -1, 17)),
		   ph::make (4, -1, 17));
}

/* Test lower_bound for unsigned C.  */

template<unsigned int N, typename C, typename T>
static void
test_unsigned_lower_bound ()
{
  typedef coeff_helper<C> ch;
  typedef poly_helper<T> ph;

  /* Test lower_bound (T, C).  */
  ASSERT_KNOWN_EQ (lower_bound (ph::make (4, -1, 3), ch::make (5)),
		   ch::make (4));
  ASSERT_KNOWN_EQ (lower_bound (ph::make (6, 5, -14), ch::make (-11)),
		   ch::make (6));

  /* Test lower_bound (C, T).  */
  ASSERT_KNOWN_EQ (lower_bound (ch::make (5), ph::make (4, -1, 3)),
		   ch::make (4));
  ASSERT_KNOWN_EQ (lower_bound (ch::make (-11), ph::make (6, 5, -14)),
		   ch::make (6));

  /* Test lower_bound (T, T).  */
  ASSERT_KNOWN_EQ (lower_bound (ph::make (4, -1, 3), ph::make (5, 7, -2)),
		   ph::make (4, 7, 3));
  ASSERT_KNOWN_EQ (lower_bound (ph::make (6, 5, -14), ph::make (-11, 4, 3)),
		   ph::make (6, 4, 3));
}

/* Test upper_bound for unsigned C.  */

template<unsigned int N, typename C, typename T>
static void
test_unsigned_upper_bound ()
{
  typedef coeff_helper<C> ch;
  typedef poly_helper<T> ph;

  /* Test upper_bound (T, C).  */
  ASSERT_KNOWN_EQ (upper_bound (ph::make (4, -1, 3), ch::make (5)),
		   ph::make (5, -1, 3));
  ASSERT_KNOWN_EQ (upper_bound (ph::make (6, 5, -14), ch::make (-11)),
		   ph::make (-11, 5, -14));

  /* Test upper_bound (C, T).  */
  ASSERT_KNOWN_EQ (upper_bound (ch::make (5), ph::make (4, -1, 3)),
		   ph::make (5, -1, 3));
  ASSERT_KNOWN_EQ (upper_bound (ch::make (-11), ph::make (6, 5, -14)),
		   ph::make (-11, 5, -14));

  /* Test upper_bound (T, T).  */
  ASSERT_KNOWN_EQ (upper_bound (ph::make (4, -1, 3), ph::make (5, 7, -2)),
		   ph::make (5, -1, -2));
  ASSERT_KNOWN_EQ (upper_bound (ph::make (6, 5, -14), ph::make (-11, 4, 3)),
		   ph::make (-11, 5, -14));
}

/* Test maybe_in_range_p for unsigned C.  */

template<unsigned int N, typename C, typename T>
static void
test_unsigned_maybe_in_range_p ()
{
  typedef coeff_helper<C> ch;
  typedef poly_helper<T> ph;

  /* Unknown size for N == 1.  */
  ASSERT_TRUE (maybe_in_range_p (ch::make (-1),
				 ch::make (0),
				 ph::make (-1, -1, -1)));
  /* Unknown size for all N.  */
  ASSERT_TRUE (maybe_in_range_p (ph::make (-1, -1, -1),
				 ch::make (0),
				 ch::make (-1)));
  /* Unknown size for N == 1.  */
  ASSERT_EQ (maybe_in_range_p (ph::make (-1, -1, -1),
			       ch::make (0),
			       ph::make (-1, -1, -1)), N == 1);
  ASSERT_EQ (maybe_in_range_p (ch::make (-2),
			       ch::make (0),
			       ph::make (-2, -2, -2)), N >= 2);
  ASSERT_FALSE (maybe_in_range_p (ph::make (-2, -2, -2),
				  ch::make (0),
				  ch::make (-2)));
  ASSERT_FALSE (maybe_in_range_p (ph::make (-2, -2, -2),
				  ch::make (0),
				  ph::make (-2, -2, -2)));
  ASSERT_TRUE (maybe_in_range_p (ph::make (-2, -2, -2),
				 ch::make (1),
				 ph::make (-2, -2, -2)));
  ASSERT_TRUE (maybe_in_range_p (ph::make (-2, -2, -2),
				 ch::make (1),
				 ch::make (-2)));
}

/* Test known_in_range_p for unsigned C.  */

template<unsigned int N, typename C, typename T>
static void
test_unsigned_known_in_range_p ()
{
  typedef coeff_helper<C> ch;
  typedef poly_helper<T> ph;

  ASSERT_FALSE (known_in_range_p (ch::make (4),
				  ph::make (5, 1, 2),
				  ch::make (-2)));
  ASSERT_TRUE (known_in_range_p (ph::make (6, 1, 2),
				 ph::make (5, 1, 2),
				 ch::make (-2)));
  ASSERT_TRUE (known_in_range_p (ph::make (6, 1, 2),
				 ph::make (5, 1, 2),
				 ph::make (-2, -2, -2)));
}

/* Test things that work for poly_int-based types T, given that the
   coefficient type C is a primitive integer type.  N is the number of
   coefficients in C */

template<unsigned int N, typename C, typename T>
static void
test_hwi ()
{
  typedef coeff_helper<C> ch;
  typedef poly_helper<T> ph;

  /* Test coeff_gcd.  */
  ASSERT_EQ (coeff_gcd (ph::make (30, 45, 10)),
	     N == 1 ? 30 : N == 2 ? 15 : 5);
  ASSERT_EQ (coeff_gcd (ph::make (0, 18, 21)),
	     N == 1 ? 0 : N == 2 ? 18 : 3);
  ASSERT_EQ (coeff_gcd (ph::make (0, 0, 101)),
	     N <= 2 ? 0 : 101);
  ASSERT_EQ (coeff_gcd (ph::make (21, 0, 28)),
	     N <= 2 ? 21 : 7);
  ASSERT_EQ (coeff_gcd (ph::make (100, 175, 0)),
	     N == 1 ? 100 : 25);

  /* Test common_multiple (T, C).  */
  ASSERT_KNOWN_EQ (common_multiple (ph::make (8, 24, 16), 6),
		   ph::make (24, 72, 48));
  ASSERT_KNOWN_EQ (common_multiple (ph::make (30, 0, 0), 45),
		   ch::make (90));
  if (N >= 2)
    ASSERT_KNOWN_EQ (common_multiple (ph::make (18, 15, 0), 12),
		     ph::make (72, 60, 0));
  if (N == 3)
    ASSERT_KNOWN_EQ (common_multiple (ph::make (18, 15, 4), 12),
		     ph::make (216, 180, 48));

  /* Test common_multiple (C, T).  */
  ASSERT_KNOWN_EQ (common_multiple (6, ph::make (8, 24, 16)),
		   ph::make (24, 72, 48));
  ASSERT_KNOWN_EQ (common_multiple (45, ph::make (30, 0, 0)),
		   ch::make (90));
  if (N >= 2)
    ASSERT_KNOWN_EQ (common_multiple (12, ph::make (18, 15, 0)),
		     ph::make (72, 60, 0));
  if (N == 3)
    ASSERT_KNOWN_EQ (common_multiple (12, ph::make (18, 15, 4)),
		     ph::make (216, 180, 48));

  /* Test force_common_multiple.  */
  ASSERT_KNOWN_EQ (force_common_multiple (ph::make (30, 0, 0),
					  ph::make (25, 0, 0)),
		   ph::make (150, 0, 0));
  if (N >= 2)
    {
      ASSERT_KNOWN_EQ (force_common_multiple (ph::make (16, 24, 0),
					      ph::make (24, 36, 0)),
		       ph::make (48, 72, 0));
      ASSERT_KNOWN_EQ (force_common_multiple (ph::make (16, 24, 0),
					      ph::make (12, 0, 0)),
		       ph::make (48, 72, 0));
      ASSERT_KNOWN_EQ (force_common_multiple (ph::make (15, 0, 0),
					      ph::make (21, 9, 0)),
		       ph::make (105, 45, 0));
    }
  if (N == 3)
    {
      ASSERT_KNOWN_EQ (force_common_multiple (ph::make (33, 99, 66),
					      ph::make (22, 66, 44)),
		       ph::make (66, 198, 132));
      ASSERT_KNOWN_EQ (force_common_multiple (ph::make (30, 0, 45),
					      ph::make (12, 0, 18)),
		       ph::make (60, 0, 90));
      ASSERT_KNOWN_EQ (force_common_multiple (ph::make (40, 0, 50),
					      ph::make (8, 0, 0)),
		       ph::make (160, 0, 200));
      ASSERT_KNOWN_EQ (force_common_multiple (ph::make (6, 0, 0),
					      ph::make (10, 0, 15)),
		       ph::make (60, 0, 90));
      ASSERT_KNOWN_EQ (force_common_multiple (ph::make (20, 40, 30),
					      ph::make (15, 0, 0)),
		       ph::make (60, 120, 90));
      ASSERT_KNOWN_EQ (force_common_multiple (ph::make (9, 0, 0),
					      ph::make (90, 81, 27)),
		       ph::make (90, 81, 27));
    }
}

/* Test poly_int<N, C>::to_shwi, using in-range source coefficient value
   SRCV (equal to DESTV) and adding DELTA to get an out-of-range value.  */

template<unsigned int N, typename C>
static void
test_to_shwi (const C &srcv, int delta, HOST_WIDE_INT destv)
{
  typedef poly_helper< poly_int<N, HOST_WIDE_INT> > ps64h;
  typedef poly_int<N, C> T;
  typedef poly_helper<T> ph;
  poly_int<N, HOST_WIDE_INT> shwi;

  /* Test in-range T::to_shwi.  */
  ASSERT_TRUE (ph::make (srcv,
			 srcv - delta,
			 srcv - delta * 2).to_shwi (&shwi));
  ASSERT_KNOWN_EQ (shwi, ps64h::make (destv,
				      destv - delta,
				      destv - delta * 2));

  /* Test partially in-range T::to_shwi.  */
  ASSERT_EQ (ph::make (srcv,
		       srcv + delta,
		       srcv + delta * 2).to_shwi (&shwi), N == 1);
  if (N == 1)
    ASSERT_KNOWN_EQ (shwi, destv);
  ASSERT_EQ (ph::make (srcv - delta,
		       srcv,
		       srcv + delta).to_shwi (&shwi), N <= 2);
  if (N <= 2)
    ASSERT_KNOWN_EQ (shwi, ps64h::make (destv - delta,
					destv,
					destv /* ignored */));

  /* Test fully out-of-range T::to_shwi.  */
  ASSERT_FALSE (ph::make (srcv + delta, srcv, srcv).to_shwi (&shwi));
}

/* Test poly_int<N, C>::to_uhwi, using in-range source coefficient value
   SRCV (equal to DESTV) and adding DELTA to get an out-of-range value.  */

template<unsigned int N, typename C>
static void
test_to_uhwi (const C &srcv, int delta, unsigned HOST_WIDE_INT destv)
{
  typedef poly_helper< poly_int<N, unsigned HOST_WIDE_INT> > pu64h;
  typedef poly_int<N, C> T;
  typedef poly_helper<T> ph;
  poly_int<N, unsigned HOST_WIDE_INT> uhwi;

  /* Test in-range T::to_uhwi.  */
  ASSERT_TRUE (ph::make (srcv,
			 srcv - delta,
			 srcv - delta * 2).to_uhwi (&uhwi));
  ASSERT_KNOWN_EQ (uhwi, pu64h::make (destv,
				      destv - delta,
				      destv - delta * 2));

  /* Test partially in-range T::to_uhwi.  */
  ASSERT_EQ (ph::make (srcv,
		       srcv + delta,
		       srcv + delta * 2).to_uhwi (&uhwi), N == 1);
  if (N == 1)
    ASSERT_KNOWN_EQ (uhwi, destv);
  ASSERT_EQ (ph::make (srcv - delta,
		       srcv,
		       srcv + delta).to_uhwi (&uhwi), N <= 2);
  if (N <= 2)
    ASSERT_KNOWN_EQ (uhwi, pu64h::make (destv - delta,
					destv,
					destv /* ignored */));

  /* Test fully out-of-range T::to_uhwi.  */
  ASSERT_FALSE (ph::make (srcv + delta, srcv, srcv).to_uhwi (&uhwi));
}

/* Test poly_int<N, C>::force_shwi and poly_int<N, C>::force_uhwi, given
   that MASK66 has the low 66 bits set and the rest clear.  */

template<unsigned int N, typename C>
static void
test_force_hwi (const C &mask66)
{
  typedef poly_helper< poly_int<N, HOST_WIDE_INT> > ps64h;
  typedef poly_helper< poly_int<N, unsigned HOST_WIDE_INT> > pu64h;
  typedef poly_int<N, C> T;
  typedef poly_helper<T> ph;
  poly_int<N, HOST_WIDE_INT> shwi;
  poly_int<N, unsigned HOST_WIDE_INT> uhwi;

  C mask65 = wi::arshift (mask66, 1);
  C mask64 = wi::arshift (mask66, 2);
  C mask63 = wi::arshift (mask66, 3);
  C mask62 = wi::arshift (mask66, 4);
  C mask61 = wi::arshift (mask66, 5);

  /* Test force_shwi.  */
  ASSERT_KNOWN_EQ (ph::make (mask66, mask65, mask64).force_shwi (),
		   ps64h::make (HOST_WIDE_INT_M1,
				HOST_WIDE_INT_M1,
				HOST_WIDE_INT_M1));
  ASSERT_KNOWN_EQ (ph::make (mask65, mask64, mask63).force_shwi (),
		   ps64h::make (HOST_WIDE_INT_M1,
				HOST_WIDE_INT_M1,
				HOST_WIDE_INT_MAX));
  ASSERT_KNOWN_EQ (ph::make (mask64, mask63, mask62).force_shwi (),
		   ps64h::make (HOST_WIDE_INT_M1,
				HOST_WIDE_INT_MAX,
				HOST_WIDE_INT_MAX / 2));
  ASSERT_KNOWN_EQ (ph::make (mask63, mask62, mask61).force_shwi (),
		   ps64h::make (HOST_WIDE_INT_MAX,
				HOST_WIDE_INT_MAX / 2,
				HOST_WIDE_INT_MAX / 4));

  /* Test force_uhwi.  */
  ASSERT_KNOWN_EQ (ph::make (mask66, mask65, mask64).force_uhwi (),
		   pu64h::make (HOST_WIDE_INT_M1U,
				HOST_WIDE_INT_M1U,
				HOST_WIDE_INT_M1U));
  ASSERT_KNOWN_EQ (ph::make (mask65, mask64, mask63).force_uhwi (),
		   pu64h::make (HOST_WIDE_INT_M1U,
				HOST_WIDE_INT_M1U,
				HOST_WIDE_INT_M1U >> 1));
  ASSERT_KNOWN_EQ (ph::make (mask64, mask63, mask62).force_uhwi (),
		   pu64h::make (HOST_WIDE_INT_M1U,
				HOST_WIDE_INT_M1U >> 1,
				HOST_WIDE_INT_M1U >> 2));
  ASSERT_KNOWN_EQ (ph::make (mask63, mask62, mask61).force_uhwi (),
		   pu64h::make (HOST_WIDE_INT_M1U >> 1,
				HOST_WIDE_INT_M1U >> 2,
				HOST_WIDE_INT_M1U >> 3));
}

/* Test poly_int<N, wide_int>::from.  */

template<unsigned int N>
static void
test_wide_int_from ()
{
  typedef poly_helper< poly_int<N, unsigned char> > pu8h;
  typedef poly_int<N, wide_int> T;
  typedef poly_helper<T> ph;

  /* Test narrowing cases of T::from.  */
  T p_8_3_1 = ph::make (wi::uhwi (8, 3),
			wi::uhwi (3, 3),
			wi::uhwi (1, 3));
  ASSERT_KNOWN_EQ (T::from (pu8h::make (0xf8,0x23,0x81), 3, SIGNED),
		   p_8_3_1);
  ASSERT_KNOWN_EQ (T::from (pu8h::make (0xf8,0x23,0x81), 3, UNSIGNED),
		   p_8_3_1);

  /* Test equal-sized cases of T::from.  */
  T p_f8_23_81 = ph::make (wi::uhwi (0xf8, 8),
			   wi::uhwi (0x23, 8),
			   wi::uhwi (0x81, 8));
  ASSERT_KNOWN_EQ (T::from (pu8h::make (0xf8,0x23,0x81), 8, SIGNED),
		   p_f8_23_81);
  ASSERT_KNOWN_EQ (T::from (pu8h::make (0xf8,0x23,0x81), 8, UNSIGNED),
		   p_f8_23_81);

  /* Test widening cases of T::from.  */
  T p_fff8_0023_ff81 = ph::make (wi::uhwi (0xfff8, 16),
				 wi::uhwi (0x0023, 16),
				 wi::uhwi (0xff81, 16));
  ASSERT_KNOWN_EQ (T::from (pu8h::make (0xf8,0x23,0x81), 16, SIGNED),
		   p_fff8_0023_ff81);
  T p_00f8_0023_0081 = ph::make (wi::uhwi (0xf8, 16),
				 wi::uhwi (0x23, 16),
				 wi::uhwi (0x81, 16));
  ASSERT_KNOWN_EQ (T::from (pu8h::make (0xf8,0x23,0x81), 16, UNSIGNED),
		   p_00f8_0023_0081);
}

/* Test wi::sext for poly_int<N, wide_int>.  */

template<unsigned int N>
static void
test_wide_int_sext ()
{
  typedef poly_int<N, wide_int> T;
  typedef poly_helper<T> ph;

  ASSERT_KNOWN_EQ (wi::sext (ph::make (wi::shwi (16, 12),
				       wi::shwi (63, 12),
				       wi::shwi (14, 12)), 5),
		   ph::make (wi::shwi (-16, 12),
			     wi::shwi (-1, 12),
			     wi::shwi (14, 12)));
  ASSERT_KNOWN_EQ (wi::sext (ph::make (wi::shwi (1024, 12),
				       wi::shwi (1023, 12),
				       wi::shwi (1200, 12)), 11),
		   ph::make (wi::shwi (-1024, 12),
			     wi::shwi (1023, 12),
			     wi::shwi (-848, 12)));
}

/* Test wi::zext for poly_int<N, wide_int>.  */

template<unsigned int N>
static void
test_wide_int_zext ()
{
  typedef poly_int<N, wide_int> T;
  typedef poly_helper<T> ph;

  ASSERT_KNOWN_EQ (wi::zext (ph::make (wi::uhwi (16, 12),
				       wi::uhwi (63, 12),
				       wi::uhwi (14, 12)), 5),
		   ph::make (wi::uhwi (16, 12),
			     wi::uhwi (31, 12),
			     wi::uhwi (14, 12)));
  ASSERT_KNOWN_EQ (wi::zext (ph::make (wi::uhwi (1024, 12),
				       wi::uhwi (1023, 12),
				       wi::uhwi (3248, 12)), 11),
		   ph::make (wi::uhwi (1024, 12),
			     wi::uhwi (1023, 12),
			     wi::uhwi (1200, 12)));
}

/* Test wi::add for poly_int<N, wide_int>.  */

template<unsigned int N>
static void
test_wide_int_add ()
{
  typedef poly_int<N, wide_int> T;
  typedef poly_helper<T> ph;

  wi::overflow_type overflow;
  ASSERT_KNOWN_EQ (wi::add (ph::make (wi::uhwi (15, 4),
				      wi::uhwi (4, 4),
				      wi::uhwi (2, 4)),
			    ph::make (wi::uhwi (1, 4),
				      wi::uhwi (0, 4),
				      wi::uhwi (0, 4)),
			    UNSIGNED, &overflow),
		   ph::make (wi::uhwi (0, 4),
			     wi::uhwi (4, 4),
			     wi::uhwi (2, 4)));
  ASSERT_TRUE ((bool)overflow);
  ASSERT_KNOWN_EQ (wi::add (ph::make (wi::uhwi (30, 5),
				      wi::uhwi (6, 5),
				      wi::uhwi (11, 5)),
			    ph::make (wi::uhwi (1, 5),
				      wi::uhwi (26, 5),
				      wi::uhwi (19, 5)),
			    UNSIGNED, &overflow),
		   ph::make (wi::uhwi (31, 5),
			     wi::uhwi (0, 5),
			     wi::uhwi (30, 5)));
  ASSERT_EQ ((bool)overflow, N >= 2);
  ASSERT_KNOWN_EQ (wi::add (ph::make (wi::uhwi (1, 6),
				      wi::uhwi (63, 6),
				      wi::uhwi (50, 6)),
			    ph::make (wi::uhwi (61, 6),
				      wi::uhwi (0, 6),
				      wi::uhwi (50, 6)),
			    UNSIGNED, &overflow),
		   ph::make (wi::uhwi (62, 6),
			     wi::uhwi (63, 6),
			     wi::uhwi (36, 6)));
  ASSERT_EQ ((bool)overflow, N == 3);

  ASSERT_KNOWN_EQ (wi::add (ph::make (wi::shwi (7, 4),
				      wi::shwi (7, 4),
				      wi::shwi (-8, 4)),
			    ph::make (wi::shwi (1, 4),
				      wi::shwi (0, 4),
				      wi::shwi (0, 4)),
			    SIGNED, &overflow),
		   ph::make (wi::shwi (-8, 4),
			     wi::shwi (7, 4),
			     wi::shwi (-8, 4)));
  ASSERT_TRUE ((bool)overflow);
  ASSERT_KNOWN_EQ (wi::add (ph::make (wi::shwi (-1, 5),
				      wi::shwi (6, 5),
				      wi::shwi (11, 5)),
			    ph::make (wi::shwi (15, 5),
				      wi::shwi (11, 5),
				      wi::shwi (-15, 5)),
			    SIGNED, &overflow),
		   ph::make (wi::shwi (14, 5),
			     wi::shwi (-15, 5),
			     wi::shwi (-4, 5)));
  ASSERT_EQ ((bool)overflow, N >= 2);
  ASSERT_KNOWN_EQ (wi::add (ph::make (wi::shwi (4, 6),
				      wi::shwi (0, 6),
				      wi::shwi (-1, 6)),
			    ph::make (wi::shwi (-32, 6),
				      wi::shwi (-32, 6),
				      wi::shwi (-32, 6)),
			    SIGNED, &overflow),
		   ph::make (wi::shwi (-28, 6),
			     wi::shwi (-32, 6),
			     wi::shwi (31, 6)));
  ASSERT_EQ ((bool)overflow, N == 3);
}

/* Test wi::sub for poly_int<N, wide_int>.  */

template<unsigned int N>
static void
test_wide_int_sub ()
{
  typedef poly_int<N, wide_int> T;
  typedef poly_helper<T> ph;

  wi::overflow_type overflow;
  ASSERT_KNOWN_EQ (wi::sub (ph::make (wi::uhwi (0, 4),
				      wi::uhwi (4, 4),
				      wi::uhwi (2, 4)),
			    ph::make (wi::uhwi (1, 4),
				      wi::uhwi (0, 4),
				      wi::uhwi (0, 4)),
			    UNSIGNED, &overflow),
		   ph::make (wi::uhwi (15, 4),
			     wi::uhwi (4, 4),
			     wi::uhwi (2, 4)));
  ASSERT_TRUE ((bool)overflow);
  ASSERT_KNOWN_EQ (wi::sub (ph::make (wi::uhwi (30, 5),
				      wi::uhwi (29, 5),
				      wi::uhwi (11, 5)),
			    ph::make (wi::uhwi (1, 5),
				      wi::uhwi (31, 5),
				      wi::uhwi (9, 5)),
			    UNSIGNED, &overflow),
		   ph::make (wi::uhwi (29, 5),
			     wi::uhwi (30, 5),
			     wi::uhwi (2, 5)));
  ASSERT_EQ ((bool)overflow, N >= 2);
  ASSERT_KNOWN_EQ (wi::sub (ph::make (wi::uhwi (0, 6),
				      wi::uhwi (63, 6),
				      wi::uhwi (0, 6)),
			    ph::make (wi::uhwi (0, 6),
				      wi::uhwi (0, 6),
				      wi::uhwi (52, 6)),
			    UNSIGNED, &overflow),
		   ph::make (wi::uhwi (0, 6),
			     wi::uhwi (63, 6),
			     wi::uhwi (12, 6)));
  ASSERT_EQ ((bool)overflow, N == 3);

  ASSERT_KNOWN_EQ (wi::sub (ph::make (wi::shwi (-8, 4),
				      wi::shwi (5, 4),
				      wi::shwi (-7, 4)),
			    ph::make (wi::shwi (1, 4),
				      wi::shwi (0, 4),
				      wi::shwi (0, 4)),
			    SIGNED, &overflow),
		   ph::make (wi::shwi (7, 4),
			     wi::shwi (5, 4),
			     wi::shwi (-7, 4)));
  ASSERT_TRUE ((bool)overflow);
  ASSERT_KNOWN_EQ (wi::sub (ph::make (wi::shwi (-1, 5),
				      wi::shwi (-7, 5),
				      wi::shwi (0, 5)),
			    ph::make (wi::shwi (15, 5),
				      wi::shwi (11, 5),
				      wi::shwi (-15, 5)),
			    SIGNED, &overflow),
		   ph::make (wi::shwi (-16, 5),
			     wi::shwi (14, 5),
			     wi::shwi (15, 5)));
  ASSERT_EQ ((bool)overflow, N >= 2);
  ASSERT_KNOWN_EQ (wi::sub (ph::make (wi::shwi (-32, 6),
				      wi::shwi (-1, 6),
				      wi::shwi (0, 6)),
			    ph::make (wi::shwi (-32, 6),
				      wi::shwi (-32, 6),
				      wi::shwi (-32, 6)),
			    SIGNED, &overflow),
		   ph::make (wi::shwi (0, 6),
			     wi::shwi (31, 6),
			     wi::shwi (-32, 6)));
  ASSERT_EQ ((bool)overflow, N == 3);
}

/* Test wi::mul for poly_int<N, wide_int>.  */

template<unsigned int N>
static void
test_wide_int_mul ()
{
  typedef poly_int<N, wide_int> T;
  typedef poly_helper<T> ph;

  wi::overflow_type overflow;
  ASSERT_KNOWN_EQ (wi::mul (ph::make (wi::uhwi (4, 4),
				      wi::uhwi (3, 4),
				      wi::uhwi (2, 4)), 4,
			    UNSIGNED, &overflow),
		   ph::make (wi::uhwi (0, 4),
			     wi::uhwi (12, 4),
			     wi::uhwi (8, 4)));
  ASSERT_TRUE ((bool)overflow);
  ASSERT_KNOWN_EQ (wi::mul (ph::make (wi::uhwi (15, 5),
				      wi::uhwi (31, 5),
				      wi::uhwi (7, 5)), 2,
			    UNSIGNED, &overflow),
		   ph::make (wi::uhwi (30, 5),
			     wi::uhwi (30, 5),
			     wi::uhwi (14, 5)));
  ASSERT_EQ ((bool)overflow, N >= 2);
  ASSERT_KNOWN_EQ (wi::mul (ph::make (wi::uhwi (1, 6),
				      wi::uhwi (0, 6),
				      wi::uhwi (2, 6)), 63,
			    UNSIGNED, &overflow),
		   ph::make (wi::uhwi (63, 6),
			     wi::uhwi (0, 6),
			     wi::uhwi (62, 6)));
  ASSERT_EQ ((bool)overflow, N == 3);

  ASSERT_KNOWN_EQ (wi::mul (ph::make (wi::shwi (-1, 4),
				      wi::shwi (1, 4),
				      wi::shwi (0, 4)), -8,
			    SIGNED, &overflow),
		   ph::make (wi::shwi (-8, 4),
			     wi::shwi (-8, 4),
			     wi::shwi (0, 4)));
  ASSERT_TRUE ((bool)overflow);
  ASSERT_KNOWN_EQ (wi::mul (ph::make (wi::shwi (2, 5),
				      wi::shwi (-3, 5),
				      wi::shwi (1, 5)), 6,
			    SIGNED, &overflow),
		   ph::make (wi::shwi (12, 5),
			     wi::shwi (14, 5),
			     wi::shwi (6, 5)));
  ASSERT_EQ ((bool)overflow, N >= 2);
  ASSERT_KNOWN_EQ (wi::mul (ph::make (wi::shwi (5, 6),
				      wi::shwi (-6, 6),
				      wi::shwi (7, 6)), -5,
			    SIGNED, &overflow),
		   ph::make (wi::shwi (-25, 6),
			     wi::shwi (30, 6),
			     wi::shwi (29, 6)));
  ASSERT_EQ ((bool)overflow, N == 3);
}

/* Test wi::neg for poly_int<N, wide_int>.  */

template<unsigned int N>
static void
test_wide_int_neg ()
{
  typedef poly_int<N, wide_int> T;
  typedef poly_helper<T> ph;

  wi::overflow_type overflow;
  ASSERT_KNOWN_EQ (wi::neg (ph::make (wi::shwi (-8, 4),
				      wi::shwi (7, 4),
				      wi::shwi (-7, 4)), &overflow),
		   ph::make (wi::shwi (-8, 4),
			     wi::shwi (-7, 4),
			     wi::shwi (7, 4)));
  ASSERT_TRUE ((bool)overflow);
  ASSERT_KNOWN_EQ (wi::neg (ph::make (wi::shwi (-15, 5),
				      wi::shwi (-16, 5),
				      wi::shwi (15, 5)), &overflow),
		   ph::make (wi::shwi (15, 5),
			     wi::shwi (-16, 5),
			     wi::shwi (-15, 5)));
  ASSERT_EQ ((bool)overflow, N >= 2);
  ASSERT_KNOWN_EQ (wi::neg (ph::make (wi::shwi (-28, 6),
				      wi::shwi (30, 6),
				      wi::shwi (-32, 6)), &overflow),
		   ph::make (wi::shwi (28, 6),
			     wi::shwi (-30, 6),
			     wi::shwi (-32, 6)));
  ASSERT_EQ ((bool)overflow, N == 3);
}

/* Test poly_int<N, C> for things that only make sense when C is an
   offset_int or widest_int.  */

template<unsigned int N, typename C>
static void
test_fixed_int (void)
{
  typedef poly_helper< poly_int<N, int> > pih;
  typedef poly_int<N, C> T;
  typedef poly_helper<T> ph;

  /* Test signed case.  */
  ASSERT_KNOWN_EQ (T::from (pih::make (-100, 200, -300), SIGNED),
		   ph::make (-100, 200, -300));
  ASSERT_MAYBE_NE (T::from (pih::make (-100, 200, -300), SIGNED),
		   ph::make (-100U, 200U, -300U));

  /* Test unsigned case.  */
  ASSERT_MAYBE_NE (T::from (pih::make (-100, 200, -300), UNSIGNED),
		   ph::make (-100, 200, -300));
  ASSERT_KNOWN_EQ (T::from (pih::make (-100, 200, -300), UNSIGNED),
		   ph::make (-100U, 200U, -300U));

  C one = 1;
  test_to_shwi<N> (-(one << 63), -1, HOST_WIDE_INT_MIN);
  test_to_shwi<N> ((one << 63) - 1, 1, HOST_WIDE_INT_MAX);
  test_to_uhwi<N> (C (0), -1, 0U);
  test_to_uhwi<N> ((one << 64) - 1, 1, HOST_WIDE_INT_M1U);

  /* Test force_shwi and force_uhwi.  */
  test_force_hwi<N> ((one << 66) - 1);
}

/* Test type promotions.  */

template<unsigned int N>
static void
test_type_promotions ()
{
  typedef poly_helper< poly_int<N, unsigned short> > pu16h;
  typedef poly_helper< poly_int<N, HOST_WIDE_INT> > ps64h;
  HOST_WIDE_INT mask32 = ~0U;

  /* Test that + on unsigned short promotes to HOST_WIDE_INT.  */
  ASSERT_KNOWN_EQ (pu16h::make (0xffff, 0xfffe, 0xfffd) + 16,
		   ps64h::make (0x1000f, 0xfffe, 0xfffd));
  ASSERT_KNOWN_EQ (32 + pu16h::make (0xffff, 0xfffe, 0xfffd),
		   ps64h::make (0x1001f, 0xfffe, 0xfffd));
  ASSERT_KNOWN_EQ (pu16h::make (0xffff, 0xfffe, 0xfffd)
		   + pu16h::make (4, 10, 17),
		   ps64h::make (0x10003, 0x10008, 0x1000e));

  /* Test that - on unsigned short promotes to HOST_WIDE_INT.  */
  ASSERT_KNOWN_EQ (pu16h::make (1, 2, 3) - ~0U,
		   ps64h::make (-mask32 + 1, 2, 3));
  ASSERT_KNOWN_EQ (INT_MIN - pu16h::make (4, 5, 6),
		   ps64h::make ((HOST_WIDE_INT) INT_MIN - 4, -5, -6));
  ASSERT_KNOWN_EQ (pu16h::make (1, 2, 3) - pu16h::make (100, 200, 300),
		   ps64h::make (-99, -198, -297));

  /* Same for unary -.  */
  ASSERT_KNOWN_EQ (-pu16h::make (0x8000, 0x9000, 0xa000),
		   ps64h::make (-0x8000, -0x9000, -0xa000));
  ASSERT_MAYBE_NE (-pu16h::make (0x8000, 0x9000, 0xa000),
		   ps64h::make (0x8000, 0x9000, 0xa000));

  /* Test that * on unsigned short promotes to HOST_WIDE_INT.  */
  ASSERT_KNOWN_EQ (pu16h::make (10, 14, 17) * ~0U,
		   ps64h::make (10 * mask32, 14 * mask32, 17 * mask32));
  ASSERT_KNOWN_EQ (-400000 * pu16h::make (10, 14, 17),
		   ps64h::make (-4000000, -5600000, -6800000));

  /* Test that << on unsigned short promotes to HOST_WIDE_INT.  */
  ASSERT_KNOWN_EQ (pu16h::make (4, 5, 6) << 50,
		   ps64h::make ((HOST_WIDE_INT) 4 << 50,
				(HOST_WIDE_INT) 5 << 50,
				(HOST_WIDE_INT) 6 << 50));

  /* Test that can_align_up doesn't truncate to the type of the alignment.  */
  poly_int<N, HOST_WIDE_INT> aligned;
  HOST_WIDE_INT a = (HOST_WIDE_INT_1 << 50);
  HOST_WIDE_INT b = (HOST_WIDE_INT_1 << 51);
  HOST_WIDE_INT c = (HOST_WIDE_INT_1 << 52);
  ASSERT_TRUE (can_align_up (ps64h::make (a - 31, b, c), 16U, &aligned));
  ASSERT_KNOWN_EQ (aligned, ps64h::make (a - 16, b, c));

  /* Likewise for can_align_down.  */
  ASSERT_TRUE (can_align_down (ps64h::make (a - 31, b, c), 16U, &aligned));
  ASSERT_KNOWN_EQ (aligned, ps64h::make (a - 32, b, c));

  /* Same for the force_* routines.  */
  ASSERT_KNOWN_EQ (force_align_up (ps64h::make (a - 31, b, c), 16U),
		   ps64h::make (a - 16, b, c));
  ASSERT_KNOWN_EQ (force_align_down (ps64h::make (a - 31, b, c), 16U),
		   ps64h::make (a - 32, b, c));

  /* Same for the aligned_*_bound routines.  */
  ASSERT_KNOWN_EQ (aligned_upper_bound (ps64h::make (a - 31, b - 33, c - 55),
					16U),
		   ps64h::make (a - 16, b - 32, c - 48));
  ASSERT_KNOWN_EQ (aligned_lower_bound (ps64h::make (a - 31, b - 33, c - 55),
					16U),
		   ps64h::make (a - 32, b - 48, c - 64));
}

/* Test endpoint_representable_p.  */

static void
test_endpoint_representable (void)
{
  /* True because the size is unknown.  */
  ASSERT_TRUE (endpoint_representable_p ((unsigned char) 0x80,
					 (unsigned char) 0xff));
  ASSERT_FALSE (endpoint_representable_p ((unsigned char) 0x80,
					  (unsigned char) 0xfe));
  ASSERT_FALSE (endpoint_representable_p ((unsigned char) 0x80,
					  (unsigned char) 0x80));
  ASSERT_TRUE (endpoint_representable_p ((unsigned char) 0x80,
					 (unsigned char) 0x7f));
  ASSERT_FALSE (endpoint_representable_p ((unsigned char) 0x11,
					  (unsigned char) 0xef));
  ASSERT_TRUE (endpoint_representable_p ((unsigned char) 0x11,
					 (unsigned char) 0xee));

  /* True because the size is unknown.  */
  ASSERT_TRUE (endpoint_representable_p (INT_MAX, -1));
  ASSERT_FALSE (endpoint_representable_p (INT_MAX - 100, INT_MAX));
  ASSERT_FALSE (endpoint_representable_p (INT_MAX - 100, 101));
  ASSERT_TRUE (endpoint_representable_p (INT_MAX - 100, 100));
  ASSERT_TRUE (endpoint_representable_p (0, INT_MAX));
  ASSERT_TRUE (endpoint_representable_p (INT_MIN, INT_MAX));

  /* True because the size is unknown.  */
  ASSERT_TRUE (endpoint_representable_p (UINT_MAX, -1U));
  ASSERT_FALSE (endpoint_representable_p (UINT_MAX - 400, UINT_MAX - 1));
  ASSERT_FALSE (endpoint_representable_p (UINT_MAX - 400, 401U));
  ASSERT_TRUE (endpoint_representable_p (UINT_MAX - 400, 400U));
}

/* Test wi::shwi with N coefficients.  */

template<unsigned int N>
static void
test_shwi ()
{
  typedef poly_int<N, wi::hwi_with_prec> T;
  typedef poly_helper<T> ph;

  poly_int<N, wide_int> mult;
  mult = ph::make (wi::shwi (80, 16),
		   wi::shwi (-10, 16),
		   wi::shwi (70, 16)) * 3;
  ASSERT_KNOWN_EQ (mult, ph::make (wi::shwi (240, 16),
				   wi::shwi (-30, 16),
				   wi::shwi (210, 16)));
}

/* Test wi::uhwi with N coefficients.  */

template<unsigned int N>
static void
test_uhwi ()
{
  typedef poly_int<N, wi::hwi_with_prec> T;
  typedef poly_helper<T> ph;

  poly_int<N, wide_int> mult;
  mult = ph::make (wi::uhwi (80, 16),
		   wi::uhwi (-10, 16),
		   wi::uhwi (70, 16)) * 3;
  ASSERT_KNOWN_EQ (mult, ph::make (wi::uhwi (240, 16),
				   wi::uhwi (-30, 16),
				   wi::uhwi (210, 16)));
}

/* Test multiple_p for non-polynomial T.  */

template<typename T>
static void
test_nonpoly_multiple_p ()
{
  ASSERT_TRUE (multiple_p (T (6), T (2)));
  ASSERT_TRUE (multiple_p (T (6), T (3)));
  ASSERT_FALSE (multiple_p (T (6), T (4)));
  ASSERT_FALSE (multiple_p (T (7), T (4)));
  ASSERT_TRUE (multiple_p (T (8), T (4)));
}

/* Test known_size_p for non-polynomial T.  */

template<typename T>
static void
test_nonpoly_known_size_p ()
{
  ASSERT_TRUE (known_size_p (T (0)));
  ASSERT_TRUE (known_size_p (T (1)));
  ASSERT_TRUE (known_size_p (T (2)));
  ASSERT_FALSE (known_size_p (T (-1)));
}

/* Test poly-int.h operations on non-polynomial type T.  */

template<typename T>
static void
test_nonpoly_type ()
{
  test_nonpoly_multiple_p<T> ();
  test_nonpoly_known_size_p<T> ();
}

/* Test poly-int.h operations on non-polynomial values.  */

static void
test_nonpoly ()
{
  test_nonpoly_type<unsigned char> ();
  test_nonpoly_type<unsigned short> ();
  test_nonpoly_type<int> ();
  test_nonpoly_type<unsigned int> ();
  test_nonpoly_type<HOST_WIDE_INT> ();
  test_nonpoly_type<unsigned HOST_WIDE_INT> ();
  test_nonpoly_type<offset_int> ();
  test_nonpoly_type<widest_int> ();
}

/* Test things that work for all poly_int-based types T, given that T
   has N coefficients of type C.  RC is the type to which C promotes
   after an operator.  */

template<unsigned int N, typename C, typename RC, typename T>
static void
test_general ()
{
  test_poly_int_traits<N, C, T> ();
  test_constants<N, C, T> ();
  test_plus_equals<N, C, T> ();
  test_minus_equals<N, C, T> ();
  test_times_equals<N, C, T> ();
  test_shl_equals<N, C, T> ();
  test_is_constant<N, C, T> ();
  test_to_constant<N, C, T> ();
  test_addition<N, C, T> ();
  test_subtraction<N, C, RC, T> ();
  test_negation<N, C, RC, T> ();
  test_multiplication<N, C, T> ();
  test_shift_left<N, C, T> ();
  test_maybe_ne<N, C, T> ();
  test_known_eq<N, C, T> ();
  test_can_align_p<N, C, T> ();
  test_can_align_up<N, C, T> ();
  test_can_align_down<N, C, T> ();
  test_known_equal_after_align_up<N, C, T> ();
  test_known_equal_after_align_down<N, C, T> ();
  test_force_align_up<N, C, T> ();
  test_force_align_down<N, C, T> ();
  test_aligned_lower_bound<N, C, T> ();
  test_aligned_upper_bound<N, C, T> ();
  test_known_misalignment<N, C, T> ();
  test_force_get_misalignment<N, C, T> ();
  test_known_alignment<N, C, T> ();
  test_can_ior_p<N, C, T> ();
  test_known_size_p<N, C, T> ();
}

/* Test things that work for poly_int<2, C>, given that C is signed.  */

template<typename C>
static void
test_ordered_2 ()
{
  test_maybe_eq_2<C> ();
  test_known_ne_2<C> ();
}

/* Test things that work for poly_int-based types T, given that the
   coefficient type C supports all the normal C operators.  N is the
   number of coefficients in C and RC is the type to which C promotes
   after an operator.  */

template<unsigned int N, typename C, typename RC, typename T>
static void
test_ordered ()
{
  test_general<N, C, RC, T> ();
  test_maybe_le<N, C, T> ();
  test_maybe_lt<N, C, T> ();
  test_maybe_ge<N, C, T> ();
  test_maybe_gt<N, C, T> ();
  test_known_gt<N, C, T> ();
  test_known_ge<N, C, T> ();
  test_known_lt<N, C, T> ();
  test_known_le<N, C, T> ();
  test_ordered_p<N, C, T> ();
  test_ordered_min<N, C, T> ();
  test_ordered_max<N, C, T> ();
  test_constant_lower_bound<N, C, T> ();
  test_lower_bound<N, C, T> ();
  test_upper_bound<N, C, T> ();
  test_compare_sizes_for_sort<N, C, T> ();
  test_force_align_up_and_div<N, C, T> ();
  test_force_align_down_and_div<N, C, T> ();
  test_constant_multiple_p<N, C, T> ();
  test_multiple_p<N, C, T> ();
  test_multiple_p_with_result<N, C, T> ();
  test_exact_div<N, C, T> ();
  test_can_div_trunc_p_const<N, C, T> ();
  test_can_div_trunc_p_poly<N, C, T> ();
  test_can_div_away_from_zero_p<N, C, T> ();
  test_maybe_in_range_p<N, C, T> ();
  test_known_in_range_p<N, C, T> ();
  test_ranges_maybe_overlap_p<N, C, T> ();
  test_ranges_known_overlap_p<N, C, T> ();
  test_known_subrange_p<N, C, T> ();
  test_coeffs_in_range_p<N, C, T> ();
}

/* Test things that work for poly_int<2, C>, given that C is signed.  */

template<typename C>
static void
test_signed_2 ()
{
  test_ordered_2<C> ();
  test_signed_maybe_eq_2<C> ();
  test_signed_known_ne_2<C> ();
}

/* Test things that work for poly_int-based types T, given that the
   coefficient type C is signed.  N is the number of coefficients in C
   and RC is the type to which C promotes after an operator.  */

template<unsigned int N, typename C, typename RC, typename T>
static void
test_signed ()
{
  test_ordered<N, C, RC, T> ();
  test_signed_negation<N, C, RC, T> ();
  test_signed_maybe_le<N, C, T> ();
  test_signed_maybe_lt<N, C, T> ();
  test_signed_maybe_ge<N, C, T> ();
  test_signed_maybe_gt<N, C, T> ();
  test_signed_known_gt<N, C, T> ();
  test_signed_known_ge<N, C, T> ();
  test_signed_known_lt<N, C, T> ();
  test_signed_known_le<N, C, T> ();
  test_signed_ordered_p<N, C, T> ();
  test_signed_ordered_min<N, C, T> ();
  test_signed_ordered_max<N, C, T> ();
  test_signed_lower_bound<N, C, T> ();
  test_signed_upper_bound<N, C, T> ();
  test_signed_constant_multiple_p<N, C, T> ();
  test_signed_multiple_p<N, C, T> ();
  test_signed_multiple_p_with_result<N ,C, T> ();
  test_signed_exact_div<N, C, T> ();
  test_signed_can_div_trunc_p_const<N, C, T> ();
  test_signed_can_div_trunc_p_poly<N, C, T> ();
  test_signed_can_div_away_from_zero_p<N, C, T> ();
  test_signed_maybe_in_range_p<N, C, T> ();
}

/* Test things that work for poly_int-based types T, given that the
   coefficient type C is unsigned.  N is the number of coefficients in C
   and RC is the type to which C promotes after an operator.  */

template<unsigned int N, typename C, typename RC, typename T>
static void
test_unsigned ()
{
  test_ordered<N, C, RC, T> ();
  test_unsigned_maybe_le<N, C, T> ();
  test_unsigned_maybe_lt<N, C, T> ();
  test_unsigned_maybe_ge<N, C, T> ();
  test_unsigned_maybe_gt<N, C, T> ();
  test_unsigned_known_gt<N, C, T> ();
  test_unsigned_known_ge<N, C, T> ();
  test_unsigned_known_lt<N, C, T> ();
  test_unsigned_known_le<N, C, T> ();
  test_unsigned_ordered_p<N, C, T> ();
  test_unsigned_ordered_min<N, C, T> ();
  test_unsigned_ordered_max<N, C, T> ();
  test_unsigned_lower_bound<N, C, T> ();
  test_unsigned_upper_bound<N, C, T> ();
  test_unsigned_maybe_in_range_p<N, C, T> ();
  test_unsigned_known_in_range_p<N, C, T> ();
}

/* Test things that are specific to coefficients of type wide_int,
   using a poly_int with N coefficients.  */

template<unsigned int N>
static void
test_wide_int ()
{
  test_wide_int_from<N> ();

  test_to_shwi<N> (wi::mask (63, true, 77), -1, HOST_WIDE_INT_MIN);
  test_to_shwi<N> (wi::mask (63, false, 77), 1, HOST_WIDE_INT_MAX);
  test_to_uhwi<N> (wide_int (wi::zero (94)), -1, 0U);
  test_to_uhwi<N> (wi::mask (64, false, 94), 1, HOST_WIDE_INT_M1U);

  test_force_hwi<N> (wi::mask (66, false, 81));

  test_wide_int_sext<N> ();
  test_wide_int_zext<N> ();
  test_wide_int_add<N> ();
  test_wide_int_sub<N> ();
  test_wide_int_mul<N> ();
  test_wide_int_neg<N> ();
}

/* Run the tests that are common to all coefficient counts N.  */

template<unsigned int N>
static void
test_num_coeffs_core ()
{
  test_unsigned<N, unsigned short, HOST_WIDE_INT,
		poly_int<N, unsigned short> > ();
  test_signed<N, HOST_WIDE_INT, HOST_WIDE_INT,
	      poly_int<N, HOST_WIDE_INT> > ();
  test_unsigned<N, unsigned HOST_WIDE_INT, unsigned HOST_WIDE_INT,
		poly_int<N, unsigned HOST_WIDE_INT> >();

  test_general<N, wide_int, wide_int, poly_int<N, wide_int> > ();

  test_hwi<N, unsigned short, poly_int<N, unsigned short> > ();
  test_hwi<N, HOST_WIDE_INT, poly_int<N, HOST_WIDE_INT> > ();
  test_hwi<N, unsigned HOST_WIDE_INT, poly_int<N, unsigned HOST_WIDE_INT> > ();

  test_wide_int<N> ();
  test_fixed_int<N, offset_int> ();
  test_fixed_int<N, widest_int> ();

  test_type_promotions<N> ();
  test_shwi<N> ();
  test_uhwi<N> ();
}

/* Run extra tests for the most important coefficient counts N.  */

template<unsigned int N>
static void
test_num_coeffs_extra ()
{
  /* Test the most common POD types.  */
  test_unsigned<N, unsigned short, HOST_WIDE_INT,
		poly_int_pod<N, unsigned short> > ();
  test_signed<N, HOST_WIDE_INT, HOST_WIDE_INT,
	      poly_int_pod<N, HOST_WIDE_INT> > ();
  test_unsigned<N, unsigned HOST_WIDE_INT, unsigned HOST_WIDE_INT,
		poly_int_pod<N, unsigned HOST_WIDE_INT> > ();

  /* Test some coefficient types that weren't covered in the core tests.  */
  test_signed<N, int, HOST_WIDE_INT,
	      poly_int<N, int> > ();
  test_signed<N, offset_int, offset_int,
	      poly_int<N, offset_int> > ();
  test_signed<N, widest_int, widest_int,
	      poly_int<N, widest_int> > ();
}
