enum E {
  A(i32),
  B(i32, i32),
}

fn foo((E::A(a) | E::B(mut a, _)): E) {}
// { dg-error "variable .a. is bound inconsistently across pattern alternatives .E0409." "" { target *-*-* } .-1 }

fn bar((E::A(a) | E::B(mut b, a)): E) {}
// { dg-error "variable .b. is not bound in all patterns .E0408." "" { target *-*-* } .-1 }

fn baz_correct((a, (E::A(c) | (E::A(c) | E::B(_, c)) | E::B(c, _)), b): (i32, E, u8)) {}

fn baz_wrong((a, (E::A(c) | (E::A(c) | E::B(_, c)) | E::B(c, z)), b): (i32, E, u8)) {}
// { dg-error "variable .z. is not bound in all patterns .E0408." "" { target *-*-* } .-1 }