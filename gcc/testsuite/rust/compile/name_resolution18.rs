struct Marker;

struct Foo {
    a: Marker,
}

pub mod foo {
    struct Foo {
        b: Marker,
    }
}

use foo::Foo; // { dg-error ".Foo. defined multiple times" }
