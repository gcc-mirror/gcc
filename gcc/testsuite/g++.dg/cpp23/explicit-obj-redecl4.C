// P0847R7
// { dg-do compile { target c++23 } }

// redeclarations with equivalent constraints

template<typename T>
concept Constrain = true;


struct S {
// xobj/static
  void f_xs_v(this S, Constrain auto) {}; // { dg-note {previous declaration} }
  static void f_xs_v(Constrain auto) {}; // { dg-error {cannot be overloaded with} }

  void f_xs_ref(this S&, Constrain auto) {}; // { dg-note {previous declaration} }
  static void f_xs_ref(Constrain auto) {}; // { dg-error {cannot be overloaded with} }

  void f_xs_cref(this S const&, Constrain auto) {}; // { dg-note {previous declaration} }
  static void f_xs_cref(Constrain auto) {}; // { dg-error {cannot be overloaded with} }

  void f_xs_rref(this S&&, Constrain auto) {}; // { dg-note {previous declaration} }
  static void f_xs_rref(Constrain auto) {}; // { dg-error {cannot be overloaded with} }

  void f_xs_crref(this S const&&, Constrain auto) {}; // { dg-note {previous declaration} }
  static void f_xs_crref(Constrain auto) {}; // { dg-error {cannot be overloaded with} }

// static/xobj
  static void f_sx_v(Constrain auto) {}; // { dg-note {previous declaration} }
  void f_sx_v(this S, Constrain auto) {}; // { dg-error {cannot be overloaded with} }

  static void f_sx_ref(Constrain auto) {}; // { dg-note {previous declaration} }
  void f_sx_ref(this S&, Constrain auto) {}; // { dg-error {cannot be overloaded with} }

  static void f_sx_cref(Constrain auto) {}; // { dg-note {previous declaration} }
  void f_sx_cref(this S const&, Constrain auto) {}; // { dg-error {cannot be overloaded with} }

  static void f_sx_rref(Constrain auto) {}; // { dg-note {previous declaration} }
  void f_sx_rref(this S&&, Constrain auto) {}; // { dg-error {cannot be overloaded with} }

  static void f_sx_crref(Constrain auto) {}; // { dg-note {previous declaration} }
  void f_sx_crref(this S const&&, Constrain auto) {}; // { dg-error {cannot be overloaded with} }

  // iobj/xobj
  void f_ix_lref(Constrain auto) {}; // { dg-note {previous declaration} }
  void f_ix_lref(this S&, Constrain auto) {}; // { dg-error {cannot be overloaded with} }

  void f_ix_rref(Constrain auto) {}; // { dg-note {previous declaration} }
  void f_ix_rref(this S&&, Constrain auto) {}; // { dg-error {cannot be overloaded with} }

  void f_ix_const_lref(Constrain auto) const {}; // { dg-note {previous declaration} }
  void f_ix_const_lref(this S const&, Constrain auto) {}; // { dg-error {cannot be overloaded with} }

  void f_ix_const_rref(Constrain auto) const {}; // { dg-note {previous declaration} }
  void f_ix_const_rref(this S const&&, Constrain auto) {}; // { dg-error {cannot be overloaded with} }
  
  // xobj/iobj
  void f_xi_lref(this S&, Constrain auto) {}; // { dg-note {previous declaration} }
  void f_xi_lref(Constrain auto) {}; // { dg-error {cannot be overloaded with} }

  void f_xi_rref(this S&&, Constrain auto) {}; // { dg-note {previous declaration} }
  void f_xi_rref(Constrain auto) {}; // { dg-error {cannot be overloaded with} }

  void f_xi_const_lref(this S const&, Constrain auto) {}; // { dg-note {previous declaration} }
  void f_xi_const_lref(Constrain auto) const {}; // { dg-error {cannot be overloaded with} }

  void f_xi_const_rref(this S const&&, Constrain auto) {}; // { dg-note {previous declaration} }
  void f_xi_const_rref(Constrain auto) const {}; // { dg-error {cannot be overloaded with} }

  // with ref qualifier

  // iobj/xobj
  void f_ix_lref_refqual(Constrain auto) & {}; // { dg-note {previous declaration} }
  void f_ix_lref_refqual(this S&, Constrain auto) {}; // { dg-error {cannot be overloaded with} }

  void f_ix_rref_refqual(Constrain auto) && {}; // { dg-note {previous declaration} }
  void f_ix_rref_refqual(this S&&, Constrain auto) {}; // { dg-error {cannot be overloaded with} }

  void f_ix_const_lref_refqual(Constrain auto) const& {}; // { dg-note {previous declaration} }
  void f_ix_const_lref_refqual(this S const&, Constrain auto) {}; // { dg-error {cannot be overloaded with} }

  void f_ix_const_rref_refqual(Constrain auto) const&& {}; // { dg-note {previous declaration} }
  void f_ix_const_rref_refqual(this S const&&, Constrain auto) {}; // { dg-error {cannot be overloaded with} }
  
  // xobj/iobj
  void f_xi_lref_refqual(this S&, Constrain auto) {}; // { dg-note {previous declaration} }
  void f_xi_lref_refqual(Constrain auto) & {}; // { dg-error {cannot be overloaded with} }

  void f_xi_rref_refqual(this S&&, Constrain auto) {}; // { dg-note {previous declaration} }
  void f_xi_rref_refqual(Constrain auto) && {}; // { dg-error {cannot be overloaded with} }

  void f_xi_const_lref_refqual(this S const&, Constrain auto) {}; // { dg-note {previous declaration} }
  void f_xi_const_lref_refqual(Constrain auto) const& {}; // { dg-error {cannot be overloaded with} }

  void f_xi_const_rref_refqual(this S const&&, Constrain auto) {}; // { dg-note {previous declaration} }
  void f_xi_const_rref_refqual(Constrain auto) const&& {}; // { dg-error {cannot be overloaded with} }
};

