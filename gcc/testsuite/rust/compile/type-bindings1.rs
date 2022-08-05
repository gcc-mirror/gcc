struct Foo<A, B>(A, B);

fn main() {
    let a;
    a = Foo::<A = i32, B = f32>(123f32);
    // { dg-error "associated type bindings are not allowed here" "" { target *-*-* } .-1 }
    // { dg-error {failed to type resolve expression} "" { target *-*-* } .-2 }
    // { dg-error {Failed to resolve expression of function call} "" { target *-*-* } .-3 }
    // { duplicate _dg-error {failed to type resolve expression} "" { target *-*-* } .-4 }
}
