#![crate_type = "lib"]

#[doc(alias = "foo")] // ok!
#[doc(alias("bar", "baz"))] // ok!
pub struct Bar;

#[doc(alias = "
")] // { dg-error "invalid character used" "" { target *-*-* } .-1 }
pub struct Foo;

#[doc(alias(
    "
"
))] // ko but unchecked for now
pub struct Foo2;

#[doc(whatever = "buidule")] // ko as well but unchecked for now
struct Boo;
