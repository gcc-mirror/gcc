unsafe fn foo() {}
unsafe fn bar() {
    foo();
}

fn main() {
    foo(); // { dg-error "call to unsafe function" }
    bar(); // { dg-error "call to unsafe function" }

    unsafe {
        foo();
        bar();
    }
}
