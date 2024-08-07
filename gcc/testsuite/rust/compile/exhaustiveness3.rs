struct S {
    e1: E1,
    e2: E2,
}

enum E1 {
    A(),
    B(),
    C(),
}

enum E2 {
    D(),
    E(),
}

// This is a valid match
fn f(s: S) {
    match s {
        S {
            e1: E1::A(),
            e2: E2::D(),
        } => {}
        S {
            e1: E1::B(),
            e2: E2::D(),
        } => {}
        S {
            e1: E1::C(),
            e2: E2::D(),
        } => {}
        S {
            e1: E1::A(),
            e2: E2::E(),
        } => {}
        S {
            e1: E1::B(),
            e2: E2::E(),
        } => {}
        S {
            e1: E1::C(),
            e2: E2::E(),
        } => {}
    }
}

fn f2(s: S) {
    match s {
        // { dg-error "non-exhaustive patterns: 'S { e1: E1::B.., e2: E2::D.. }' and 'S { e1: E1::C.., e2: E2::D.. }' not covered" "" { target *-*-* } .-1 }
        S { e1: E1::A(), e2: _ } => {}
        S { e1: _, e2: E2::E() } => {}
    }
}

fn main() {}
