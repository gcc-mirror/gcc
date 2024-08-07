enum E1 {
    E2(E2),
    None,
}

enum E2 {
    E3(E3),
    None,
}

enum E3 {
    S(S),
    None,
}

struct S {
    a: i32,
    b: u64,
}

fn f1(e: E1) {
    match e {
        // { dg-error "non-exhaustive patterns: 'E1::E2.E2::None.' and 'E1::None' not covered" "" { target *-*-* } .-1 }
        E1::E2(E2::E3(_)) => {}
    }
}

fn main() {}
