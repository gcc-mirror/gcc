struct Foo;
fn Foo() {} // { dg-error ".Foo. defined multiple times" }

struct Marker;
struct Bar {
    a: Marker,
}
fn Bar() {} // ok, since `Bar` is not a value here
