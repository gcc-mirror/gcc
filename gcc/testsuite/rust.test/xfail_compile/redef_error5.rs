// { dg-excess-errors "Noisy error and debug" }
struct Foo(i32, bool);

impl Foo {
    const TEST: i32 = 123; // { dg-error "was defined here"  }
    const TEST: bool = false; // { dg-error "redefined multiple times"  }
}

fn main() {}
