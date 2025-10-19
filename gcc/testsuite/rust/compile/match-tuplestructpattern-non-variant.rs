enum Empty {}
enum NonEmpty {
    Foo(i32),
}

fn f(e: Empty) {
    match e { 
        Empty(0) => {} // { dg-error "expected tuple struct or tuple variant, found enum 'Empty'" }
    }

    match e {
        Empty(Empty(..)) => {} // { dg-error "expected tuple struct or tuple variant, found enum 'Empty'" }
    }
}

fn g(e: NonEmpty) {
    match e {
        NonEmpty(0) => {} // { dg-error "expected tuple struct or tuple variant, found enum 'NonEmpty'" }
    }
}
