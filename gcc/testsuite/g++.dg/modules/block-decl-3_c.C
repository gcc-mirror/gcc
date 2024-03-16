// { dg-module-do link }
// { dg-additional-options "-fmodules-ts" }

import mod;

int main() {
  n_n().f();
  n_i().f();
  i_n().f();
  i_i().f();

  n_n_n().f().g();
  n_i_n().f().g();
  i_n_i().f().g();
  i_i_i().f().g();

  multi_n_n().x.f();
  multi_n_i().x.f();
  multi_i_i().x.f();

  extern_n_i().f();
  extern_i_i().f();

  gmf_n_i().f();
  gmf_i_i().f();
  gmf_n_i_i().f().g();
  gmf_i_i_i().f().g();

  test_from_impl_unit();
}
