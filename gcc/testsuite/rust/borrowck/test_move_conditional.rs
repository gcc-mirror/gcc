// { dg-additional-options "-frust-compile-until=compilation -frust-borrowcheck" }

fn test_move_conditional(b1: bool, b2:bool) { // { dg-error "Found move errors in function test_move" }
    struct A {
        i: i32,
    }

    let a = A { i: 1 };
    let b = a;
    if b1 {
        let b = a;
    }
    if b2 {
        let c = a;
    }
}

fn test_move_fixed(b1: bool, b2:bool) {

    let a = 1; // a is now primitive and can be copied
    let b = a;
    if b1 {
        let b = a;
    }
    if b2 {
        let c = a;
    }
}