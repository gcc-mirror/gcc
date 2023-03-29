fn foo() {
    8;
    8;
}

fn bar() -> i32 {
    8;
    8
}

fn baz() -> i32 {
    8;
    return 8;
}

fn main() {
    let a = foo(); // { dg-warning "unused name" }
    let b = bar(); // { dg-warning "unused name" }
    let c = baz(); // { dg-warning "unused name" }
}
