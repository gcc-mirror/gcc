// { dg-additional-options "-fmodules-ts" }

import M;

int main() {
  // These are all OK
  function();
  int a = var;
  klass k;
  klass_tmpl<int> kt;
  klass_tmpl<int*> ktp;
  no_odr_use();

  function_tmpl<ok_inst_tag>();
  function_tmpl<ok_inst_tag*>();
  int b = var_tmpl<ok_inst_tag>;
  int c = var_tmpl<ok_inst_tag*>;
  int d = *ptr;
  int e = dynamic_var;

  // But don't ignore exposures in these cases
  function_tmpl<int>();  // { dg-message "required from here" }
  int x = var_tmpl<int>;  // { dg-message "required from here" }
  int y = var_tmpl<int*>;  // { dg-message "required from here" }
  int z = *ptr_tmpl<int>;  // { dg-message "required from here" }

  // And decls initialized to a TU-local value are not constant here
  // Unfortunately the error does not currently point to this decl
  constexpr int& r = constant_ref;
  // { dg-error "is not a constant expression" "" { target *-*-* } 0 }
}

// The errors occur in a different file, so we just test that all the
// needed "required from here"s are found above.
// { dg-error "instantiation exposes TU-local entity" "" { target *-*-* } 0 }
// { dg-bogus "required from here" "" { target *-*-* } 0 }
