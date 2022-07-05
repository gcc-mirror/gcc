struct Foo<const N>; // { dg-error "expecting .:. but .>. found" }
struct Bar<const N: >; // { dg-error "unrecognised token .>. in type" }
struct Baz<const N: usize = >; // { dg-error "invalid token for start of default value for const generic parameter" }
// { dg-error "unrecognised token .>. in type" "" { target *-*-* } .-1 }
