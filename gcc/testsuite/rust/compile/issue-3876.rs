enum test {
    A(i32),
}

fn fun(x: i32) {
    test::A { x }
    // { dg-error "unknown field" "" { target *-*-* } .-1 }
}
