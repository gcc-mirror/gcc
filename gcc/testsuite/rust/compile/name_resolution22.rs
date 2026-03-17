#![feature(no_core)]
#![no_core]

struct Marker;
struct Foo(Marker);

fn Foo(m: Marker) {} // { dg-error ".Foo. defined multiple times" }
