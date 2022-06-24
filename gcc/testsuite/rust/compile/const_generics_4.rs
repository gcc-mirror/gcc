// { dg-additional-options "-w" }

const P: usize = 14;

struct Foo<const N: usize = { M }>; // { dg-error "failed to find name: M" }
struct Bar<const N: usize = { P }>;
struct Baz<const N: NotAType = { P }>; // { dg-error "failed to resolve TypePath: NotAType in this scope" }
