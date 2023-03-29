struct Foo {
    one: i32,
// { dg-warning "field is never read" "" { target *-*-* } .-1 }
    two: i32
}
fn main() {
    let _a = Foo {one: 1, two: 2};
    let _b = _a.two;
}