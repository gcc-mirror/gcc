#![feature(rustc_attrs)]

#[rustc_const_unstable(feature = "const_ascii_ctype_on_intrinsics", issue = "1234")]
pub fn foo() {}
