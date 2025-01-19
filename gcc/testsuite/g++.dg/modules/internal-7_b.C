// { dg-additional-options "-fmodules-ts" }

module M;

void inst() {
  expose_1<int>();  // { dg-message "required from here" }
  expose_2<int>();  // { dg-message "required from here" }
  expose_3<int>();  // { dg-message "required from here" }
  expose_4<int>();  // { dg-message "required from here" }
  expose_5<int>();  // { dg-message "required from here" }
  expose_6<int>();  // { dg-message "required from here" }
  expose_7<int>();  // { dg-message "required from here" }
  expose_8<int>();  // { dg-message "required from here" }
  expose_9<int>();  // { dg-message "required from here" }
  expose_10<int>();  // { dg-message "required from here" }
  expose_11<int>();  // { dg-message "required from here" }

  expose_var<int>;  // { dg-message "required from here" }
}

// { dg-error "instantiation exposes TU-local entity" "" { target *-*-* } 0 }
