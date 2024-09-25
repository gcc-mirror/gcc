// { dg-options "-frust-name-resolution-2.0" }
struct Marker;
struct Foo(Marker);

fn Foo(m: Marker) {} // { dg-error ".Foo. defined multiple times" }
