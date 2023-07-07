fn foo() {
    1 = 3; // { dg-error "invalid left-hand side of assignment" }
}

fn main() {
    foo();
}