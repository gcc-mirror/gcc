
fn bar() { // {dg-warning "function is never used: `bar`"}
    foo();
}

fn foo() { // {dg-warning "function is never used: `foo`"}
    bar();
}

fn f() {

}

fn main() {
    f();
}