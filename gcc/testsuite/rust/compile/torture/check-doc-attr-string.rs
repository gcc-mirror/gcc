#![crate_type = "lib"]

#[doc(alias = "foo")] // ok!
#[doc(alias("bar", "baz"))] // ok!
pub struct Bar;

#[doc(alias = "
")] // { dg-error "unended string literal" "" { target *-*-* } .-1 }
pub struct Foo;

#[doc(alias("
"))] // { dg-error "unended string literal" "" { target *-*-* } .-1 }
pub struct Foo2;
