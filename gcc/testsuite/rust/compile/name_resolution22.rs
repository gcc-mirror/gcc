struct Marker;
struct Foo(Marker);

fn Foo(m: Marker) {} // { dg-error ".Foo. defined multiple times" }
