// https://doc.rust-lang.org/error_codes/E0124.html
fn main() {
    struct Foo {
        field1: i32, // { dg-error "field .field1. is already declared" }
        field1: i32, // { dg-error "field .field1. is already declared" }
        field1: i32, // { dg-error "field .field1. is already declared" }
    }
}
