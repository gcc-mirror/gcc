// { dg-additional-options "-frust-compile-until=compilation -frust-borrowcheck" }
fn test_move_behind_reference() {
    // { dg-error "Cannot move from behind a reference." "" { target *-*-* } .-1 }
    struct A {
        i: i32,
    }
    struct B {
        a: A,
    }
    let a = A { i: 1 };
    let b = B { a };
    let c = &b;
    let d = c.a;
}

fn test_move_behind_reference_fixed() {
    struct A {
        i: i32,
    }
    struct B {
        a: A,
    }
    let a = A { i: 1 };
    let b = B { a };
    let c = b;
    let d = c.a;
}
