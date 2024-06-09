// P0847R7
// { dg-do run { target c++23 } }

// overload resolution of static/xobj and iobj/xobj member functions
// with constraints

template<typename T>
concept Constrain = true;

inline constexpr int iobj_fn = 5;
inline constexpr int xobj_fn = 10;
inline constexpr int static_fn = 20;

// first 2 letters are the order of the definitions
// the constraint applies to the first definition,
// for *_r cases the constraint applies to the second

struct S {
  // xobj/static
  int f_xs_v(this S, Constrain auto) { return xobj_fn; };
  static int f_xs_v(auto) { return static_fn; };

  int f_xs_ref(this S&, Constrain auto) { return xobj_fn; };
  static int f_xs_ref(auto) { return static_fn; };

  int f_xs_cref(this S const&, Constrain auto) { return xobj_fn; };
  static int f_xs_cref(auto) { return static_fn; };

  int f_xs_rref(this S&&, Constrain auto) { return xobj_fn; };
  static int f_xs_rref(auto) { return static_fn; };

  int f_xs_crref(this S const&&, Constrain auto) { return xobj_fn; };
  static int f_xs_crref(auto) { return static_fn; };

  // _r
  int f_xs_v_r(this S, auto) { return xobj_fn; };
  static int f_xs_v_r(Constrain auto) { return static_fn; };

  int f_xs_ref_r(this S&, auto) { return xobj_fn; };
  static int f_xs_ref_r(Constrain auto) { return static_fn; };

  int f_xs_cref_r(this S const&, auto) { return xobj_fn; };
  static int f_xs_cref_r(Constrain auto) { return static_fn; };

  int f_xs_rref_r(this S&&, auto) { return xobj_fn; };
  static int f_xs_rref_r(Constrain auto) { return static_fn; };

  int f_xs_crref_r(this S const&&, auto) { return xobj_fn; };
  static int f_xs_crref_r(Constrain auto) { return static_fn; };

  // static/xobj
  static int f_sx_v(Constrain auto) { return static_fn; };
  int f_sx_v(this S, auto) { return xobj_fn; };

  static int f_sx_ref(Constrain auto) { return static_fn; };
  int f_sx_ref(this S&, auto) { return xobj_fn; };

  static int f_sx_cref(Constrain auto) { return static_fn; };
  int f_sx_cref(this S const&, auto) { return xobj_fn; };

  static int f_sx_rref(Constrain auto) { return static_fn; };
  int f_sx_rref(this S&&, auto) { return xobj_fn; };

  static int f_sx_crref(Constrain auto) { return static_fn; };
  int f_sx_crref(this S const&&, auto) { return xobj_fn; };

  // _r
  static int f_sx_v_r(auto) { return static_fn; };
  int f_sx_v_r(this S, Constrain auto) { return xobj_fn; };

  static int f_sx_ref_r(auto) { return static_fn; };
  int f_sx_ref_r(this S&, Constrain auto) { return xobj_fn; };

  static int f_sx_cref_r(auto) { return static_fn; };
  int f_sx_cref_r(this S const&, Constrain auto) { return xobj_fn; };

  static int f_sx_rref_r(auto) { return static_fn; };
  int f_sx_rref_r(this S&&, Constrain auto) { return xobj_fn; };

  static int f_sx_crref_r(auto) { return static_fn; };
  int f_sx_crref_r(this S const&&, Constrain auto) { return xobj_fn; };

  // xobj/iobj with matching object parameters

  // We are only testing constraints here, so we need parameter lists
  // to match, which means we need corresponding object parameters.
  // Remember, the rules for object parameter correspondence are weird.
  // ([basic.scope.scope-3.1])

  // *_refqual the iobj member function has a reference qualifier
  // *_r the constraint applies to the second definition

  // ix
  int f_ix_m0(Constrain auto) { return iobj_fn; };
  int f_ix_m0(this S&, auto) { return xobj_fn; };
  // See note
  int f_ix_m1(Constrain auto) { return iobj_fn; };
  int f_ix_m1(this S&&, auto) { return xobj_fn; };

  int f_ix_c0(Constrain auto) const { return iobj_fn; };
  int f_ix_c0(this S const&, auto) { return xobj_fn; };
  // See note
  int f_ix_c1(Constrain auto) const { return iobj_fn; };
  int f_ix_c1(this S const&&, auto) { return xobj_fn; };
  
  // xi
  int f_xi_m0(this S&, Constrain auto) { return xobj_fn; };
  int f_xi_m0(auto) { return iobj_fn; };
  // See note
  int f_xi_m1(this S&&, Constrain auto) { return xobj_fn; };
  int f_xi_m1(auto) { return iobj_fn; };

  int f_xi_c0(this S const&, Constrain auto) { return xobj_fn; };
  int f_xi_c0(auto) const { return iobj_fn; };
  // See note
  int f_xi_c1(this S const&&, Constrain auto) { return xobj_fn; };
  int f_xi_c1(auto) const { return iobj_fn; };

  // with ref qualifier

  // ix
  int f_ix_m0_refqual(Constrain auto) & { return iobj_fn; };
  int f_ix_m0_refqual(this S&, auto) { return xobj_fn; };

  int f_ix_m1_refqual(Constrain auto) && { return iobj_fn; };
  int f_ix_m1_refqual(this S&&, auto) { return xobj_fn; };

  int f_ix_c0_refqual(Constrain auto) const& { return iobj_fn; };
  int f_ix_c0_refqual(this S const&, auto) { return xobj_fn; };

  int f_ix_c1_refqual(Constrain auto) const&& { return iobj_fn; };
  int f_ix_c1_refqual(this S const&&, auto) { return xobj_fn; };
  
  // xi 
  int f_xi_m0_refqual(this S&, Constrain auto) { return xobj_fn; };
  int f_xi_m0_refqual(auto) & { return iobj_fn; };

  int f_xi_m1_refqual(this S&&, Constrain auto) { return xobj_fn; };
  int f_xi_m1_refqual(auto) && { return iobj_fn; };

  int f_xi_c0_refqual(this S const&, Constrain auto) { return xobj_fn; };
  int f_xi_c0_refqual(auto) const& { return iobj_fn; };

  int f_xi_c1_refqual(this S const&&, Constrain auto) { return xobj_fn; };
  int f_xi_c1_refqual(auto) const&& { return iobj_fn; };

  // _r without ref qualifier

  // ix
  int f_ix_m0_r(auto) { return iobj_fn; };
  int f_ix_m0_r(this S&, Constrain auto) { return xobj_fn; };
  // See note
  int f_ix_m1_r(auto) { return iobj_fn; };
  int f_ix_m1_r(this S&&, Constrain auto) { return xobj_fn; };

  int f_ix_c0_r(auto) const { return iobj_fn; };
  int f_ix_c0_r(this S const&, Constrain auto) { return xobj_fn; };
  // See note
  int f_ix_c1_r(auto) const { return iobj_fn; };
  int f_ix_c1_r(this S const&&, Constrain auto) { return xobj_fn; };
  
  // xi
  int f_xi_m0_r(this S&, auto) { return xobj_fn; };
  int f_xi_m0_r(Constrain auto) { return iobj_fn; };
  // See note
  int f_xi_m1_r(this S&&, auto) { return xobj_fn; };
  int f_xi_m1_r(Constrain auto) { return iobj_fn; };

  int f_xi_c0_r(this S const&, auto) { return xobj_fn; };
  int f_xi_c0_r(Constrain auto) const { return iobj_fn; };
  // See note
  int f_xi_c1_r(this S const&&, auto) { return xobj_fn; };
  int f_xi_c1_r(Constrain auto) const { return iobj_fn; };

  // _r with ref qualifier
  // ix
  int f_ix_m0_refqual_r(auto) & { return iobj_fn; };
  int f_ix_m0_refqual_r(this S&, Constrain auto) { return xobj_fn; };

  int f_ix_m1_refqual_r(auto) && { return iobj_fn; };
  int f_ix_m1_refqual_r(this S&&, Constrain auto) { return xobj_fn; };

  int f_ix_c0_refqual_r(auto) const& { return iobj_fn; };
  int f_ix_c0_refqual_r(this S const&, Constrain auto) { return xobj_fn; };

  int f_ix_c1_refqual_r(auto) const&& { return iobj_fn; };
  int f_ix_c1_refqual_r(this S const&&, Constrain auto) { return xobj_fn; };
  
  // xi
  int f_xi_m0_refqual_r(this S&, auto) { return xobj_fn; };
  int f_xi_m0_refqual_r(Constrain auto) & { return iobj_fn; };

  int f_xi_m1_refqual_r(this S&&, auto) { return xobj_fn; };
  int f_xi_m1_refqual_r(Constrain auto) && { return iobj_fn; };

  int f_xi_c0_refqual_r(this S const&, auto) { return xobj_fn; };
  int f_xi_c0_refqual_r(Constrain auto) const& { return iobj_fn; };

  int f_xi_c1_refqual_r(this S const&&, auto) { return xobj_fn; };
  int f_xi_c1_refqual_r(Constrain auto) const&& { return iobj_fn; };
};


int main()
{
  // The commented out cases are ambiguous, which is most likely the correct
  // behavior. It is something that I want to propose to change, and I want
  // to leave them in as they are a little weird.
  //
  // Furthermore, as the comment at the top of this file indicates, I am not
  // clear on the correct behavior of the static/xobj cases in general.

  S s{};
  if (s.f_xs_v (0) != xobj_fn)
    __builtin_abort ();
  if (s.f_xs_ref (0) != xobj_fn)
    __builtin_abort ();
  if (s.f_xs_cref (0) != xobj_fn)
    __builtin_abort ();
  if (static_cast<S&&>(s).f_xs_rref (0) != xobj_fn)
    __builtin_abort ();
  if (static_cast<S&&>(s).f_xs_crref (0) != xobj_fn)
    __builtin_abort ();
  // if (s.f_xs_dv (0) != xobj_fn)
  //   __builtin_abort ();
  // if (s.f_xs_dcref (0) != xobj_fn)
  //   __builtin_abort ();
  // if (s.f_xs_dfwdref (0) != xobj_fn)
  //   __builtin_abort ();

  if (s.f_xs_v_r (0) != static_fn)
    __builtin_abort ();
  if (s.f_xs_ref_r (0) != static_fn)
    __builtin_abort ();
  if (s.f_xs_cref_r (0) != static_fn)
    __builtin_abort ();
  if (static_cast<S&&>(s).f_xs_rref_r (0) != static_fn)
    __builtin_abort ();
  if (static_cast<S&&>(s).f_xs_crref_r (0) != static_fn)
    __builtin_abort ();
  // if (s.f_xs_dv_r (0) != static_fn)
  //   __builtin_abort ();
  // if (s.f_xs_dcref_r (0) != static_fn)
  //   __builtin_abort ();
  // if (s.f_xs_dfwdref_r (0) != static_fn)
  //   __builtin_abort ();

  if (s.f_sx_v (0) != static_fn)
    __builtin_abort ();
  if (s.f_sx_ref (0) != static_fn)
    __builtin_abort ();
  if (s.f_sx_cref (0) != static_fn)
    __builtin_abort ();
  if (static_cast<S&&>(s).f_sx_rref (0) != static_fn)
    __builtin_abort ();
  if (static_cast<S&&>(s).f_sx_crref (0) != static_fn)
    __builtin_abort ();
  // if (s.f_sx_dv (0) != static_fn)
  //   __builtin_abort ();
  // if (s.f_sx_dcref (0) != static_fn)
  //   __builtin_abort ();
  // if (s.f_sx_dfwdref (0) != static_fn)
  //   __builtin_abort ();

  if (s.f_sx_v_r (0) != xobj_fn)
    __builtin_abort ();
  if (s.f_sx_ref_r (0) != xobj_fn)
    __builtin_abort ();
  if (s.f_sx_cref_r (0) != xobj_fn)
    __builtin_abort ();
  if (static_cast<S&&>(s).f_sx_rref_r (0) != xobj_fn)
    __builtin_abort ();
  if (static_cast<S&&>(s).f_sx_crref_r (0) != xobj_fn)
    __builtin_abort ();
  // if (s.f_sx_dv_r (0) != xobj_fn)
  //   __builtin_abort ();
  // if (s.f_sx_dcref_r (0) != xobj_fn)
  //   __builtin_abort ();
  // if (s.f_sx_dfwdref_r (0) != xobj_fn)
  //   __builtin_abort ();

  // iobj/xobj

  // The commented out cases are tested below as their correct behavior is
  // unintuitive, see the note below for details.
  
  if (s.f_ix_m0 (0) != iobj_fn)
    __builtin_abort ();
  // s.f_ix_m1
  if (s.f_ix_c0 (0) != iobj_fn)
    __builtin_abort ();
  // s.f_ix_c1
  if (s.f_xi_m0 (0) != xobj_fn)
    __builtin_abort ();
  // s.f_xi_m1
  if (s.f_xi_c0 (0) != xobj_fn)
    __builtin_abort ();
  // s.f_xi_c1
  if (s.f_ix_m0_refqual (0) != iobj_fn)
    __builtin_abort ();
  if (static_cast<S&&>(s).f_ix_m1_refqual (0) != iobj_fn)
    __builtin_abort ();
  if (s.f_ix_c0_refqual (0) != iobj_fn)
    __builtin_abort ();
  if (static_cast<S&&>(s).f_ix_c1_refqual (0) != iobj_fn)
    __builtin_abort ();
  if (s.f_xi_m0_refqual (0) != xobj_fn)
    __builtin_abort ();
  if (static_cast<S&&>(s).f_xi_m1_refqual (0) != xobj_fn)
    __builtin_abort ();
  if (s.f_xi_c0_refqual (0) != xobj_fn)
    __builtin_abort ();
  if (static_cast<S&&>(s).f_xi_c1_refqual (0) != xobj_fn)
    __builtin_abort ();
  if (s.f_ix_m0_r (0) != xobj_fn)
    __builtin_abort ();
  // s.f_ix_m1_r
  if (s.f_ix_c0_r (0) != xobj_fn)
    __builtin_abort ();
  // s.f_ix_c1_r
  if (s.f_xi_m0_r (0) != iobj_fn)
    __builtin_abort ();
  // s.f_xi_m1_r
  if (s.f_xi_c0_r (0) != iobj_fn)
    __builtin_abort ();
  // s.f_xi_c1_r
  if (s.f_ix_m0_refqual_r (0) != xobj_fn)
    __builtin_abort ();
  if (static_cast<S&&>(s).f_ix_m1_refqual_r (0) != xobj_fn)
    __builtin_abort ();
  if (s.f_ix_c0_refqual_r (0) != xobj_fn)
    __builtin_abort ();
  if (static_cast<S&&>(s).f_ix_c1_refqual_r (0) != xobj_fn)
    __builtin_abort ();
  if (s.f_xi_m0_refqual_r (0) != iobj_fn)
    __builtin_abort ();
  if (static_cast<S&&>(s).f_xi_m1_refqual_r (0) != iobj_fn)
    __builtin_abort ();
  if (s.f_xi_c0_refqual_r (0) != iobj_fn)
    __builtin_abort ();
  if (static_cast<S&&>(s).f_xi_c1_refqual_r (0) != iobj_fn)
    __builtin_abort ();


/* Note:
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
  if (s.f_ix_m1 (0) != iobj_fn)
    __builtin_abort ();
  if (s.f_ix_c1 (0) != iobj_fn)
    __builtin_abort ();
  if (s.f_xi_m1 (0) != iobj_fn)
    __builtin_abort ();
  if (s.f_xi_c1 (0) != iobj_fn)
    __builtin_abort ();

  if (s.f_ix_m1_r (0) != iobj_fn)
    __builtin_abort ();
  if (s.f_ix_c1_r (0) != iobj_fn)
    __builtin_abort ();
  if (s.f_xi_m1_r (0) != iobj_fn)
    __builtin_abort ();
  if (s.f_xi_c1_r (0) != iobj_fn)
    __builtin_abort ();

  // Constraints are taken into account here, see note for more information.
  if (static_cast<S&&>(s).f_ix_m1 (0) != iobj_fn)
    __builtin_abort ();
  if (static_cast<S&&>(s).f_ix_c1 (0) != iobj_fn)
    __builtin_abort ();
  if (static_cast<S&&>(s).f_xi_m1 (0) != xobj_fn)
    __builtin_abort ();
  if (static_cast<S&&>(s).f_xi_c1 (0) != xobj_fn)
    __builtin_abort ();

  if (static_cast<S&&>(s).f_ix_m1_r (0) != xobj_fn)
    __builtin_abort ();
  if (static_cast<S&&>(s).f_ix_c1_r (0) != xobj_fn)
    __builtin_abort ();
  if (static_cast<S&&>(s).f_xi_m1_r (0) != iobj_fn)
    __builtin_abort ();
  if (static_cast<S&&>(s).f_xi_c1_r (0) != iobj_fn)
    __builtin_abort ();
}

