struct S {
    a: i32,
}

fn s1(s: S) {
    match s {
        S { a: _ } => {}
    }
}

fn s2(s: S) {
    match s {
        _ => {}
    }
}

fn s3(s: S) {
    match s {}
}

enum E {
    A(),
    B(),
    C(),
}

fn e1(e: E) {
    match e {
        // { dg-error "non-exhaustive patterns: 'E::B..' not covered" "" { target *-*-* } .-1 }
        E::A() => {}
        E::C() => {}
    }
}

fn e2(e: E) {
    match e {
        // { dg-error "non-exhaustive patterns: 'E::A..' not covered" "" { target *-*-* } .-1 }
        E::B() => {}
        E::C() => {}
    }
}

fn e3(e: E) {
    match e {
        E::A() => {}
        E::B() => {}
        E::C() => {}
    }
}

fn main() {}
