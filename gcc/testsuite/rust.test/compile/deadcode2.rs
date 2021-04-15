fn foo() -> i32 {
    return 1;

    let a = -1; // { dg-warning "unreachable statement" }
    a // { dg-warning "unreachable expression" }
}

fn main() {
    foo();
}
