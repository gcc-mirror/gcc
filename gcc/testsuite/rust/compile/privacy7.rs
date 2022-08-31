pub struct Foo(i8);
struct Bar(pub i8); // { dg-warning "struct is never constructed: .Bar." }
pub struct Baz {
    a: i32, // { dg-warning "field is never read: .a." }
    pub b: i32,
}

pub fn foo() {}
fn bar() {} // { dg-warning "function is never used: .bar." }
