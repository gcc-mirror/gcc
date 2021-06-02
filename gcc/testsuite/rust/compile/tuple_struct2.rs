struct Bar(i32, i32, bool);

fn main() {
    let a = Bar(1, 2); // { dg-error "unexpected number of arguments 2 expected 3" }
    // { dg-error "failed to lookup type to CallExpr" "" { target *-*-* } .-1 }
    // { dg-error "failed to type resolve expression" "" { target *-*-* } .-2 }
}
