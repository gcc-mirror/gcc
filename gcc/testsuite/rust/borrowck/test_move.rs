// { dg-additional-options "-frust-compile-until=compilation -frust-borrowcheck" }
fn test_move() { // { dg-error "Found move errors in function test_move" }
    struct A {
        i: i32,
    }
    let a = A { i: 1 };
    let b = a;
    let c = a;
}

fn test_move_fixed() {

    let a = 1; // a is now primitive and can be copied
    let b = a;
    let c = b;
}