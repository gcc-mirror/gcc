struct Foo(bool);
fn foo(_: usize) -> Foo {
    Foo(false)
}

fn main() {
    match Foo(true) {
        // { dg-error "failed to type resolve expression" "" { target *-*-* } .-1 }
        foo(x)
        // { dg-error "expected tuple struct/variant, found" "" { target *-*-* } .-1 }
        => ()
    }
}
