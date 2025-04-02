trait Foo {
    type T;
    fn foo() -> Foo<main>;
    // { dg-error "generic arguments are not allowed for this type .E0109." "" { target *-*-* } .-1 }
}

fn main() {}
