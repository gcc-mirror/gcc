struct Foo(i32, bool);

impl Foo {
    const TEST: i32 = 123;
    const TEST: bool = false; // { dg-error "redefined multiple times"  }
}

fn main() {}
