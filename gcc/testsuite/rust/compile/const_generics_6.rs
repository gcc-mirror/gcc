struct Foo<const N: usize>;
struct Bar<const N: usize = { 15i32 }>; // { dg-error "mismatched types, expected .usize. but got .i32." }
