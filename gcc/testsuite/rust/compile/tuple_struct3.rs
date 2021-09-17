struct Foo(i32, i32, bool);

fn main() {
    let c = Foo(1, 2f32, true);
    // { dg-error "expected .i32. got .f32." "" { target *-*-* } .-1 }
    // { dg-error "unexpected number of arguments 1 expected 3" "" { target *-*-* } .-2 }
    // { dg-error "failed to lookup type to CallExpr" "" { target *-*-* } .-3 }
    // { dg-error "failed to type resolve expression" "" { target *-*-* } .-4 }
}
