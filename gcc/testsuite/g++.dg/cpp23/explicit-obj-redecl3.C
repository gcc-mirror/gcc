// P0847R7
// { dg-do compile { target c++23 } }

// redeclarations of xobj member functions as static member functions and vice versa

struct S {
// no additional params
  void f_xs_v(this S) {}; // { dg-note {previous declaration} }
  static void f_xs_v() {}; // { dg-error {cannot be overloaded with} }

  void f_xs_ref(this S&) {}; // { dg-note {previous declaration} }
  static void f_xs_ref() {}; // { dg-error {cannot be overloaded with} }

  void f_xs_cref(this S const&) {}; // { dg-note {previous declaration} }
  static void f_xs_cref() {}; // { dg-error {cannot be overloaded with} }

  void f_xs_rref(this S&&) {}; // { dg-note {previous declaration} }
  static void f_xs_rref() {}; // { dg-error {cannot be overloaded with} }

  void f_xs_crref(this S const&&) {}; // { dg-note {previous declaration} }
  static void f_xs_crref() {}; // { dg-error {cannot be overloaded with} }

// reversed
  static void f_sx_v() {}; // { dg-note {previous declaration} }
  void f_sx_v(this S) {}; // { dg-error {cannot be overloaded with} }

  static void f_sx_ref() {}; // { dg-note {previous declaration} }
  void f_sx_ref(this S&) {}; // { dg-error {cannot be overloaded with} }

  static void f_sx_cref() {}; // { dg-note {previous declaration} }
  void f_sx_cref(this S const&) {}; // { dg-error {cannot be overloaded with} }

  static void f_sx_rref() {}; // { dg-note {previous declaration} }
  void f_sx_rref(this S&&) {}; // { dg-error {cannot be overloaded with} }

  static void f_sx_crref() {}; // { dg-note {previous declaration} }
  void f_sx_crref(this S const&&) {}; // { dg-error {cannot be overloaded with} }

// one additional param
  void f_xs_v_int(this S, int) {}; // { dg-note {previous declaration} }
  static void f_xs_v_int(int) {}; // { dg-error {cannot be overloaded with} }

  void f_xs_ref_int(this S&, int) {}; // { dg-note {previous declaration} }
  static void f_xs_ref_int(int) {}; // { dg-error {cannot be overloaded with} }

  void f_xs_cref_int(this S const&, int) {}; // { dg-note {previous declaration} }
  static void f_xs_cref_int(int) {}; // { dg-error {cannot be overloaded with} }

  void f_xs_rref_int(this S&&, int) {}; // { dg-note {previous declaration} }
  static void f_xs_rref_int(int) {}; // { dg-error {cannot be overloaded with} }

  void f_xs_crref_int(this S const&&, int) {}; // { dg-note {previous declaration} }
  static void f_xs_crref_int(int) {}; // { dg-error {cannot be overloaded with} }

// reversed
  static void f_sx_v_int(int) {}; // { dg-note {previous declaration} }
  void f_sx_v_int(this S, int) {}; // { dg-error {cannot be overloaded with} }

  static void f_sx_ref_int(int) {}; // { dg-note {previous declaration} }
  void f_sx_ref_int(this S&, int) {}; // { dg-error {cannot be overloaded with} }

  static void f_sx_cref_int(int) {}; // { dg-note {previous declaration} }
  void f_sx_cref_int(this S const&, int) {}; // { dg-error {cannot be overloaded with} }

  static void f_sx_rref_int(int) {}; // { dg-note {previous declaration} }
  void f_sx_rref_int(this S&&, int) {}; // { dg-error {cannot be overloaded with} }

  static void f_sx_crref_int(int) {}; // { dg-note {previous declaration} }
  void f_sx_crref_int(this S const&&, int) {}; // { dg-error {cannot be overloaded with} }

// two additional params
  void f_xs_v_int2(this S, int, int) {}; // { dg-note {previous declaration} }
  static void f_xs_v_int2(int, int) {}; // { dg-error {cannot be overloaded with} }

  void f_xs_ref_int2(this S&, int, int) {}; // { dg-note {previous declaration} }
  static void f_xs_ref_int2(int, int) {}; // { dg-error {cannot be overloaded with} }

  void f_xs_cref_int2(this S const&, int, int) {}; // { dg-note {previous declaration} }
  static void f_xs_cref_int2(int, int) {}; // { dg-error {cannot be overloaded with} }

  void f_xs_rref_int2(this S&&, int, int) {}; // { dg-note {previous declaration} }
  static void f_xs_rref_int2(int, int) {}; // { dg-error {cannot be overloaded with} }

  void f_xs_crref_int2(this S const&&, int, int) {}; // { dg-note {previous declaration} }
  static void f_xs_crref_int2(int, int) {}; // { dg-error {cannot be overloaded with} }

// reversed
  static void f_sx_v_int2(int, int) {}; // { dg-note {previous declaration} }
  void f_sx_v_int2(this S, int, int) {}; // { dg-error {cannot be overloaded with} }

  static void f_sx_ref_int2(int, int) {}; // { dg-note {previous declaration} }
  void f_sx_ref_int2(this S&, int, int) {}; // { dg-error {cannot be overloaded with} }

  static void f_sx_cref_int2(int, int) {}; // { dg-note {previous declaration} }
  void f_sx_cref_int2(this S const&, int, int) {}; // { dg-error {cannot be overloaded with} }

  static void f_sx_rref_int2(int, int) {}; // { dg-note {previous declaration} }
  void f_sx_rref_int2(this S&&, int, int) {}; // { dg-error {cannot be overloaded with} }

  static void f_sx_crref_int2(int, int) {}; // { dg-note {previous declaration} }
  void f_sx_crref_int2(this S const&&, int, int) {}; // { dg-error {cannot be overloaded with} }
};

// unrelated explicit object parameter type

struct A {};

struct S1
{
// no additional params
  void f_xs_v(this A) {}; // { dg-note {previous declaration} }
  static void f_xs_v() {}; // { dg-error {cannot be overloaded with} }

  void f_xs_ref(this A&) {}; // { dg-note {previous declaration} }
  static void f_xs_ref() {}; // { dg-error {cannot be overloaded with} }

  void f_xs_cref(this A const&) {}; // { dg-note {previous declaration} }
  static void f_xs_cref() {}; // { dg-error {cannot be overloaded with} }

  void f_xs_rref(this A&&) {}; // { dg-note {previous declaration} }
  static void f_xs_rref() {}; // { dg-error {cannot be overloaded with} }

  void f_xs_crref(this A const&&) {}; // { dg-note {previous declaration} }
  static void f_xs_crref() {}; // { dg-error {cannot be overloaded with} }

// reversed
  static void f_sx_v() {}; // { dg-note {previous declaration} }
  void f_sx_v(this A) {}; // { dg-error {cannot be overloaded with} }

  static void f_sx_ref() {}; // { dg-note {previous declaration} }
  void f_sx_ref(this A&) {}; // { dg-error {cannot be overloaded with} }

  static void f_sx_cref() {}; // { dg-note {previous declaration} }
  void f_sx_cref(this A const&) {}; // { dg-error {cannot be overloaded with} }

  static void f_sx_rref() {}; // { dg-note {previous declaration} }
  void f_sx_rref(this A&&) {}; // { dg-error {cannot be overloaded with} }

  static void f_sx_crref() {}; // { dg-note {previous declaration} }
  void f_sx_crref(this A const&&) {}; // { dg-error {cannot be overloaded with} }

// one additional param
  void f_xs_v_int(this A, int) {}; // { dg-note {previous declaration} }
  static void f_xs_v_int(int) {}; // { dg-error {cannot be overloaded with} }

  void f_xs_ref_int(this A&, int) {}; // { dg-note {previous declaration} }
  static void f_xs_ref_int(int) {}; // { dg-error {cannot be overloaded with} }

  void f_xs_cref_int(this A const&, int) {}; // { dg-note {previous declaration} }
  static void f_xs_cref_int(int) {}; // { dg-error {cannot be overloaded with} }

  void f_xs_rref_int(this A&&, int) {}; // { dg-note {previous declaration} }
  static void f_xs_rref_int(int) {}; // { dg-error {cannot be overloaded with} }

  void f_xs_crref_int(this A const&&, int) {}; // { dg-note {previous declaration} }
  static void f_xs_crref_int(int) {}; // { dg-error {cannot be overloaded with} }

// reversed
  static void f_sx_v_int(int) {}; // { dg-note {previous declaration} }
  void f_sx_v_int(this A, int) {}; // { dg-error {cannot be overloaded with} }

  static void f_sx_ref_int(int) {}; // { dg-note {previous declaration} }
  void f_sx_ref_int(this A&, int) {}; // { dg-error {cannot be overloaded with} }

  static void f_sx_cref_int(int) {}; // { dg-note {previous declaration} }
  void f_sx_cref_int(this A const&, int) {}; // { dg-error {cannot be overloaded with} }

  static void f_sx_rref_int(int) {}; // { dg-note {previous declaration} }
  void f_sx_rref_int(this A&&, int) {}; // { dg-error {cannot be overloaded with} }

  static void f_sx_crref_int(int) {}; // { dg-note {previous declaration} }
  void f_sx_crref_int(this A const&&, int) {}; // { dg-error {cannot be overloaded with} }

// two additional params
  void f_xs_v_int2(this A, int, int) {}; // { dg-note {previous declaration} }
  static void f_xs_v_int2(int, int) {}; // { dg-error {cannot be overloaded with} }

  void f_xs_ref_int2(this A&, int, int) {}; // { dg-note {previous declaration} }
  static void f_xs_ref_int2(int, int) {}; // { dg-error {cannot be overloaded with} }

  void f_xs_cref_int2(this A const&, int, int) {}; // { dg-note {previous declaration} }
  static void f_xs_cref_int2(int, int) {}; // { dg-error {cannot be overloaded with} }

  void f_xs_rref_int2(this A&&, int, int) {}; // { dg-note {previous declaration} }
  static void f_xs_rref_int2(int, int) {}; // { dg-error {cannot be overloaded with} }

  void f_xs_crref_int2(this A const&&, int, int) {}; // { dg-note {previous declaration} }
  static void f_xs_crref_int2(int, int) {}; // { dg-error {cannot be overloaded with} }

// reversed
  static void f_sx_v_int2(int, int) {}; // { dg-note {previous declaration} }
  void f_sx_v_int2(this A, int, int) {}; // { dg-error {cannot be overloaded with} }

  static void f_sx_ref_int2(int, int) {}; // { dg-note {previous declaration} }
  void f_sx_ref_int2(this A&, int, int) {}; // { dg-error {cannot be overloaded with} }

  static void f_sx_cref_int2(int, int) {}; // { dg-note {previous declaration} }
  void f_sx_cref_int2(this A const&, int, int) {}; // { dg-error {cannot be overloaded with} }

  static void f_sx_rref_int2(int, int) {}; // { dg-note {previous declaration} }
  void f_sx_rref_int2(this A&&, int, int) {}; // { dg-error {cannot be overloaded with} }

  static void f_sx_crref_int2(int, int) {}; // { dg-note {previous declaration} }
  void f_sx_crref_int2(this A const&&, int, int) {}; // { dg-error {cannot be overloaded with} }
};

