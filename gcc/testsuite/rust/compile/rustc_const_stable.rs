#[rustc_const_stable(feature = "const_ascii_ctype_on_intrinsics", since = "1.47.0")]
pub fn foo() {} // { dg-error "macro not found" "" { target *-*-* } .-1 }
