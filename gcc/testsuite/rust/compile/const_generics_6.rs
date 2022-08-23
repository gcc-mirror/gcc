struct Foo<const N: usize>;
struct Bar<const N: usize = { 15i32 }>; // { dg-error "expected .usize. got .i32." }
