// P0847R7
// { dg-do run { target c++23 } }

// overload resolution of static/xobj and iobj/xobj non-template member functions
// with constraints in a class template

template<typename T>
concept Constrain = true;

inline constexpr int iobj_fn = 5;
inline constexpr int xobj_fn = 10;
inline constexpr int static_fn = 20;

// first 2 letters are the order of the definitions
// the constraint applies to the first definition,
// for *_r cases the constraint applies to the second

template<typename T>
struct S {
  // xobj/static
  int f_xs_v(this S) requires Constrain<T> { return xobj_fn; };
  static int f_xs_v() { return static_fn; };

  int f_xs_ref(this S&) requires Constrain<T> { return xobj_fn; };
  static int f_xs_ref() { return static_fn; };

  int f_xs_cref(this S const&) requires Constrain<T> { return xobj_fn; };
  static int f_xs_cref() { return static_fn; };

  int f_xs_rref(this S&&) requires Constrain<T> { return xobj_fn; };
  static int f_xs_rref() { return static_fn; };

  int f_xs_crref(this S const&&) requires Constrain<T> { return xobj_fn; };
  static int f_xs_crref() { return static_fn; };

  int f_xs_dv(this auto) requires Constrain<T> { return xobj_fn; };
  static int f_xs_dv() { return static_fn; };

  int f_xs_dcref(this auto const&) requires Constrain<T> { return xobj_fn; };
  static int f_xs_dcref() { return static_fn; };

  int f_xs_dfwdref(this auto&&) requires Constrain<T> { return xobj_fn; };
  static int f_xs_dfwdref() { return static_fn; };

  // _r
  int f_xs_v_r(this S) { return xobj_fn; };
  static int f_xs_v_r() requires Constrain<T> { return static_fn; };

  int f_xs_ref_r(this S&) { return xobj_fn; };
  static int f_xs_ref_r() requires Constrain<T> { return static_fn; };

  int f_xs_cref_r(this S const&) { return xobj_fn; };
  static int f_xs_cref_r() requires Constrain<T> { return static_fn; };

  int f_xs_rref_r(this S&&) { return xobj_fn; };
  static int f_xs_rref_r() requires Constrain<T> { return static_fn; };

  int f_xs_crref_r(this S const&&) { return xobj_fn; };
  static int f_xs_crref_r() requires Constrain<T> { return static_fn; };

  int f_xs_dv_r(this auto) { return xobj_fn; };
  static int f_xs_dv_r() requires Constrain<T> { return static_fn; };

  int f_xs_dcref_r(this auto const&) { return xobj_fn; };
  static int f_xs_dcref_r() requires Constrain<T> { return static_fn; };

  int f_xs_dfwdref_r(this auto&&) { return xobj_fn; };
  static int f_xs_dfwdref_r() requires Constrain<T> { return static_fn; };

  // static/xobj
  static int f_sx_v() requires Constrain<T> { return static_fn; };
  int f_sx_v(this S) { return xobj_fn; };

  static int f_sx_ref() requires Constrain<T> { return static_fn; };
  int f_sx_ref(this S&) { return xobj_fn; };

  static int f_sx_cref() requires Constrain<T> { return static_fn; };
  int f_sx_cref(this S const&) { return xobj_fn; };

  static int f_sx_rref() requires Constrain<T> { return static_fn; };
  int f_sx_rref(this S&&) { return xobj_fn; };

  static int f_sx_crref() requires Constrain<T> { return static_fn; };
  int f_sx_crref(this S const&&) { return xobj_fn; };

  static int f_sx_dv() requires Constrain<T> { return static_fn; };
  int f_sx_dv(this auto) { return xobj_fn; };

  static int f_sx_dcref() requires Constrain<T> { return static_fn; };
  int f_sx_dcref(this auto const&) { return xobj_fn; };

  static int f_sx_dfwdref() requires Constrain<T> { return static_fn; };
  int f_sx_dfwdref(this auto&&) { return xobj_fn; };

  // _r
  static int f_sx_v_r() { return static_fn; };
  int f_sx_v_r(this S) requires Constrain<T> { return xobj_fn; };

  static int f_sx_ref_r() { return static_fn; };
  int f_sx_ref_r(this S&) requires Constrain<T> { return xobj_fn; };

  static int f_sx_cref_r() { return static_fn; };
  int f_sx_cref_r(this S const&) requires Constrain<T> { return xobj_fn; };

  static int f_sx_rref_r() { return static_fn; };
  int f_sx_rref_r(this S&&) requires Constrain<T> { return xobj_fn; };

  static int f_sx_crref_r() { return static_fn; };
  int f_sx_crref_r(this S const&&) requires Constrain<T> { return xobj_fn; };

  static int f_sx_dv_r() { return static_fn; };
  int f_sx_dv_r(this auto) requires Constrain<T> { return xobj_fn; };

  static int f_sx_dcref_r() { return static_fn; };
  int f_sx_dcref_r(this auto const&) requires Constrain<T> { return xobj_fn; };

  static int f_sx_dfwdref_r() { return static_fn; };
  int f_sx_dfwdref_r(this auto&&) requires Constrain<T> { return xobj_fn; };

  // xobj/iobj with matching object parameters

  // We are only testing constraints here, so we need parameter lists
  // to match, which means we need corresponding object parameters.
  // Remember, the rules for object parameter correspondence are weird.
  // ([basic.scope.scope-3.1])
  //
  // NOTE: CWG2789 does not specify this properly, I am implementing it
  // assuming the above correspondence rules

  // *_refqual the iobj member function has a reference qualifier
  // *_r the constraint applies to the second definition

  // ix
  int f_ix_m0() requires Constrain<T> { return iobj_fn; };
  int f_ix_m0(this S&) { return xobj_fn; };
  // See note
  int f_ix_m1() requires Constrain<T> { return iobj_fn; };
  int f_ix_m1(this S&&) { return xobj_fn; };

  int f_ix_c0() const requires Constrain<T> { return iobj_fn; };
  int f_ix_c0(this S const&) { return xobj_fn; };
  // See note
  int f_ix_c1() const requires Constrain<T> { return iobj_fn; };
  int f_ix_c1(this S const&&) { return xobj_fn; };
  
  // xi
  int f_xi_m0(this S&) requires Constrain<T> { return xobj_fn; };
  int f_xi_m0() { return iobj_fn; };
  // See note
  int f_xi_m1(this S&&) requires Constrain<T> { return xobj_fn; };
  int f_xi_m1() { return iobj_fn; };

  int f_xi_c0(this S const&) requires Constrain<T> { return xobj_fn; };
  int f_xi_c0() const { return iobj_fn; };
  // See note
  int f_xi_c1(this S const&&) requires Constrain<T> { return xobj_fn; };
  int f_xi_c1() const { return iobj_fn; };

  // with ref qualifier

  // ix
  int f_ix_m0_refqual() & requires Constrain<T> { return iobj_fn; };
  int f_ix_m0_refqual(this S&) { return xobj_fn; };

  int f_ix_m1_refqual() && requires Constrain<T> { return iobj_fn; };
  int f_ix_m1_refqual(this S&&) { return xobj_fn; };

  int f_ix_c0_refqual() const& requires Constrain<T> { return iobj_fn; };
  int f_ix_c0_refqual(this S const&) { return xobj_fn; };

  int f_ix_c1_refqual() const&& requires Constrain<T> { return iobj_fn; };
  int f_ix_c1_refqual(this S const&&) { return xobj_fn; };
  
  // xi 
  int f_xi_m0_refqual(this S&) requires Constrain<T> { return xobj_fn; };
  int f_xi_m0_refqual() & { return iobj_fn; };

  int f_xi_m1_refqual(this S&&) requires Constrain<T> { return xobj_fn; };
  int f_xi_m1_refqual() && { return iobj_fn; };

  int f_xi_c0_refqual(this S const&) requires Constrain<T> { return xobj_fn; };
  int f_xi_c0_refqual() const& { return iobj_fn; };

  int f_xi_c1_refqual(this S const&&) requires Constrain<T> { return xobj_fn; };
  int f_xi_c1_refqual() const&& { return iobj_fn; };

  // _r without ref qualifier

  // ix
  int f_ix_m0_r() { return iobj_fn; };
  int f_ix_m0_r(this S&) requires Constrain<T> { return xobj_fn; };
  // See note
  int f_ix_m1_r() { return iobj_fn; };
  int f_ix_m1_r(this S&&) requires Constrain<T> { return xobj_fn; };

  int f_ix_c0_r() const { return iobj_fn; };
  int f_ix_c0_r(this S const&) requires Constrain<T> { return xobj_fn; };
  // See note
  int f_ix_c1_r() const { return iobj_fn; };
  int f_ix_c1_r(this S const&&) requires Constrain<T> { return xobj_fn; };
  
  // xi
  int f_xi_m0_r(this S&) { return xobj_fn; };
  int f_xi_m0_r() requires Constrain<T> { return iobj_fn; };
  // See note
  int f_xi_m1_r(this S&&) { return xobj_fn; };
  int f_xi_m1_r() requires Constrain<T> { return iobj_fn; };

  int f_xi_c0_r(this S const&) { return xobj_fn; };
  int f_xi_c0_r() const requires Constrain<T> { return iobj_fn; };
  // See note
  int f_xi_c1_r(this S const&&) { return xobj_fn; };
  int f_xi_c1_r() const requires Constrain<T> { return iobj_fn; };

  // _r with ref qualifier
  // ix
  int f_ix_m0_refqual_r() & { return iobj_fn; };
  int f_ix_m0_refqual_r(this S&) requires Constrain<T> { return xobj_fn; };

  int f_ix_m1_refqual_r() && { return iobj_fn; };
  int f_ix_m1_refqual_r(this S&&) requires Constrain<T> { return xobj_fn; };

  int f_ix_c0_refqual_r() const& { return iobj_fn; };
  int f_ix_c0_refqual_r(this S const&) requires Constrain<T> { return xobj_fn; };

  int f_ix_c1_refqual_r() const&& { return iobj_fn; };
  int f_ix_c1_refqual_r(this S const&&) requires Constrain<T> { return xobj_fn; };
  
  // xi
  int f_xi_m0_refqual_r(this S&) { return xobj_fn; };
  int f_xi_m0_refqual_r() & requires Constrain<T> { return iobj_fn; };

  int f_xi_m1_refqual_r(this S&&) { return xobj_fn; };
  int f_xi_m1_refqual_r() && requires Constrain<T> { return iobj_fn; };

  int f_xi_c0_refqual_r(this S const&) { return xobj_fn; };
  int f_xi_c0_refqual_r() const& requires Constrain<T> { return iobj_fn; };

  int f_xi_c1_refqual_r(this S const&&) { return xobj_fn; };
  int f_xi_c1_refqual_r() const&& requires Constrain<T> { return iobj_fn; };
};

int main()
{
  // The commented out cases are ambiguous, which is most likely the correct
  // behavior. It is something that I want to propose to change, and I want
  // to leave them in as they are a little weird.
  //
  // Furthermore, as the comment at the top of this file indicates, I am not
  // clear on the correct behavior of the static/xobj cases in general.
  using S = S<int>;
  S s{};
  if (s.f_xs_v () != xobj_fn)
    __builtin_abort ();
  if (s.f_xs_ref () != xobj_fn)
    __builtin_abort ();
  if (s.f_xs_cref () != xobj_fn)
    __builtin_abort ();
  if (static_cast<S&&>(s).f_xs_rref () != xobj_fn)
    __builtin_abort ();
  if (static_cast<S&&>(s).f_xs_crref () != xobj_fn)
    __builtin_abort ();
  // if (s.f_xs_dv () != xobj_fn)
  //   __builtin_abort ();
  // if (s.f_xs_dcref () != xobj_fn)
  //   __builtin_abort ();
  // if (s.f_xs_dfwdref () != xobj_fn)
  //   __builtin_abort ();

  if (s.f_xs_v_r () != static_fn)
    __builtin_abort ();
  if (s.f_xs_ref_r () != static_fn)
    __builtin_abort ();
  if (s.f_xs_cref_r () != static_fn)
    __builtin_abort ();
  if (static_cast<S&&>(s).f_xs_rref_r () != static_fn)
    __builtin_abort ();
  if (static_cast<S&&>(s).f_xs_crref_r () != static_fn)
    __builtin_abort ();
  // if (s.f_xs_dv_r () != static_fn)
  //   __builtin_abort ();
  // if (s.f_xs_dcref_r () != static_fn)
  //   __builtin_abort ();
  // if (s.f_xs_dfwdref_r () != static_fn)
  //   __builtin_abort ();

  if (s.f_sx_v () != static_fn)
    __builtin_abort ();
  if (s.f_sx_ref () != static_fn)
    __builtin_abort ();
  if (s.f_sx_cref () != static_fn)
    __builtin_abort ();
  if (static_cast<S&&>(s).f_sx_rref () != static_fn)
    __builtin_abort ();
  if (static_cast<S&&>(s).f_sx_crref () != static_fn)
    __builtin_abort ();
  // if (s.f_sx_dv () != static_fn)
  //   __builtin_abort ();
  // if (s.f_sx_dcref () != static_fn)
  //   __builtin_abort ();
  // if (s.f_sx_dfwdref () != static_fn)
  //   __builtin_abort ();

  if (s.f_sx_v_r () != xobj_fn)
    __builtin_abort ();
  if (s.f_sx_ref_r () != xobj_fn)
    __builtin_abort ();
  if (s.f_sx_cref_r () != xobj_fn)
    __builtin_abort ();
  if (static_cast<S&&>(s).f_sx_rref_r () != xobj_fn)
    __builtin_abort ();
  if (static_cast<S&&>(s).f_sx_crref_r () != xobj_fn)
    __builtin_abort ();
  // if (s.f_sx_dv_r () != xobj_fn)
  //   __builtin_abort ();
  // if (s.f_sx_dcref_r () != xobj_fn)
  //   __builtin_abort ();
  // if (s.f_sx_dfwdref_r () != xobj_fn)
  //   __builtin_abort ();

  // iobj/xobj

  // The commented out cases are tested below as their correct behavior is
  // unintuitive, see the note below for details.
  
  if (s.f_ix_m0 () != iobj_fn)
    __builtin_abort ();
  // s.f_ix_m1
  if (s.f_ix_c0 () != iobj_fn)
    __builtin_abort ();
  // s.f_ix_c1
  if (s.f_xi_m0 () != xobj_fn)
    __builtin_abort ();
  // s.f_xi_m1
  if (s.f_xi_c0 () != xobj_fn)
    __builtin_abort ();
  // s.f_xi_c1
  if (s.f_ix_m0_refqual () != iobj_fn)
    __builtin_abort ();
  if (static_cast<S&&>(s).f_ix_m1_refqual () != iobj_fn)
    __builtin_abort ();
  if (s.f_ix_c0_refqual () != iobj_fn)
    __builtin_abort ();
  if (static_cast<S&&>(s).f_ix_c1_refqual () != iobj_fn)
    __builtin_abort ();
  if (s.f_xi_m0_refqual () != xobj_fn)
    __builtin_abort ();
  if (static_cast<S&&>(s).f_xi_m1_refqual () != xobj_fn)
    __builtin_abort ();
  if (s.f_xi_c0_refqual () != xobj_fn)
    __builtin_abort ();
  if (static_cast<S&&>(s).f_xi_c1_refqual () != xobj_fn)
    __builtin_abort ();
  if (s.f_ix_m0_r () != xobj_fn)
    __builtin_abort ();
  // s.f_ix_m1_r
  if (s.f_ix_c0_r () != xobj_fn)
    __builtin_abort ();
  // s.f_ix_c1_r
  if (s.f_xi_m0_r () != iobj_fn)
    __builtin_abort ();
  // s.f_xi_m1_r
  if (s.f_xi_c0_r () != iobj_fn)
    __builtin_abort ();
  // s.f_xi_c1_r
  if (s.f_ix_m0_refqual_r () != xobj_fn)
    __builtin_abort ();
  if (static_cast<S&&>(s).f_ix_m1_refqual_r () != xobj_fn)
    __builtin_abort ();
  if (s.f_ix_c0_refqual_r () != xobj_fn)
    __builtin_abort ();
  if (static_cast<S&&>(s).f_ix_c1_refqual_r () != xobj_fn)
    __builtin_abort ();
  if (s.f_xi_m0_refqual_r () != iobj_fn)
    __builtin_abort ();
  if (static_cast<S&&>(s).f_xi_m1_refqual_r () != iobj_fn)
    __builtin_abort ();
  if (s.f_xi_c0_refqual_r () != iobj_fn)
    __builtin_abort ();
  if (static_cast<S&&>(s).f_xi_c1_refqual_r () != iobj_fn)
    __builtin_abort ();


/* Foreword: CWG2789 does not specifiy this correctly, it needs to be changed
   to consider correspondence instead of "same type" or else the following
   does not make sense. My implementation assumes correspondence should be
   considered.

   Note:
   These cases are weird, the object argument correspond, but are not the same
   type ([basic.scope.scope-3.1]), so we get this funny edge case where the
   constraint stops them from being considered redeclarations, but isn't taken
   into account for the lvalue case. You can't bind an lvalue to an rvalue
   reference so the iobj member function is always taken regardless of which
   overload is constrained.

   [over.match.funcs.general-4]
   For implicit object member functions, the type of the implicit object
   parameter is
   (4.1) “lvalue reference to cv X” for functions declared without a
	 ref-qualifier or with the & ref-qualifier

   You would think that calling these functions with an rvalue would be the
   same then, always taking the xobj member function. However, for backwards
   compatibility reasons, an unqualified member function can be called on an
   object that is an rvalue.

   [over.match.funcs.general-5]
   For implicit object member functions declared without a ref-qualifier, even
   if the implicit object parameter is not const-qualified, an rvalue can be
   bound to the parameter as long as in all other respects the argument can be
   converted to the type of the implicit object parameter.

   And finally, since the object parameters correspond ([basic.scope.scope-3.1])
   the constraints are taken into account.

   So in conclusion, calling these functions with an lvalue always resolves to
   the iobj member function, and calling them with rvalues take the constraints
   into account.

   As wacky as this is, this is the correct behavior.  */

  // Always takes the iobj member function, can't bind an lvalue to an rvalue
  // reference.
  if (s.f_ix_m1 () != iobj_fn)
    __builtin_abort ();
  if (s.f_ix_c1 () != iobj_fn)
    __builtin_abort ();
  if (s.f_xi_m1 () != iobj_fn)
    __builtin_abort ();
  if (s.f_xi_c1 () != iobj_fn)
    __builtin_abort ();

  if (s.f_ix_m1_r () != iobj_fn)
    __builtin_abort ();
  if (s.f_ix_c1_r () != iobj_fn)
    __builtin_abort ();
  if (s.f_xi_m1_r () != iobj_fn)
    __builtin_abort ();
  if (s.f_xi_c1_r () != iobj_fn)
    __builtin_abort ();

  // Constraints are taken into account here, see note for more information.
  if (static_cast<S&&>(s).f_ix_m1 () != iobj_fn)
    __builtin_abort ();
  if (static_cast<S&&>(s).f_ix_c1 () != iobj_fn)
    __builtin_abort ();
  if (static_cast<S&&>(s).f_xi_m1 () != xobj_fn)
    __builtin_abort ();
  if (static_cast<S&&>(s).f_xi_c1 () != xobj_fn)
    __builtin_abort ();

  if (static_cast<S&&>(s).f_ix_m1_r () != xobj_fn)
    __builtin_abort ();
  if (static_cast<S&&>(s).f_ix_c1_r () != xobj_fn)
    __builtin_abort ();
  if (static_cast<S&&>(s).f_xi_m1_r () != iobj_fn)
    __builtin_abort ();
  if (static_cast<S&&>(s).f_xi_c1_r () != iobj_fn)
    __builtin_abort ();
}

