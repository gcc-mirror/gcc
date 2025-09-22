enum A {
    Value(()),
}

fn main() {
    let a = A::Value(());
    a == A::Value;
    // { dg-error "variant expected constructor call" "" { target *-*-* } .-1 }
}
