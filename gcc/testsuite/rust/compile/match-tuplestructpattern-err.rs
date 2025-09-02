fn main() {
    struct A (i32, i32);
    let a = A (0, 1);

    match a {
        A (1, 2, 3, 4) => {},
        // { dg-error "this pattern has 4 fields but the corresponding tuple variant has 2 fields .E0023." "" { target *-*-* } .-1 }
        A (1, 2, .., 3, 4) => {},
        // { dg-error "this pattern has 4 fields but the corresponding tuple variant has 2 fields .E0023." "" { target *-*-* } .-1 }
        A (.., 3, 4, 5) => {},
        // { dg-error "this pattern has 3 fields but the corresponding tuple variant has 2 fields .E0023." "" { target *-*-* } .-1 }
        _ => {}
    }
}