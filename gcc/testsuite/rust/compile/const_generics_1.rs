// { dg-additional-options "-w" }

// There are errors about unused generic parameters, but we can't handle that yet.
// Still, this code is invalid Rust.

mod sain {
    struct Foo<const N: usize>;
    struct Bar<T, const N: usize>;
    struct Baz<'l, T, const N: usize>;
}

mod doux {
    struct Foo<const N: usize = 15>;
    struct Bar<T, const N: usize = { 14 * 2 }>;

    const N_DEFAULT: usize = 3;

    struct Baz<'l, T, const N: usize = N_DEFAULT>;
}
