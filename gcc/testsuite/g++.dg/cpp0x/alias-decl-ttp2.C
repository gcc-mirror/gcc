// PR c++/104107
// { dg-do compile { target c++11 } }

template<int targ_id, typename t_parameter,
	 template<typename T, const T&> class tt_w_data>
struct tt_main {
  static t_parameter m_parameter;
  template<template<typename T, const T&> class t_data>
    using t_make = t_data<t_parameter, m_parameter>;
  using t_data = t_make<tt_w_data>;
};

template<int targ_id, typename t_parameter,
	 template<typename T, const T&> class tt_w_data>
t_parameter tt_main<targ_id, t_parameter, tt_w_data>::m_parameter;

template<typename T, const T&> struct P {};
struct t_parameter {};

using toto = tt_main<0, t_parameter, P>;

int main() {
  toto t;
  return 0;
}
