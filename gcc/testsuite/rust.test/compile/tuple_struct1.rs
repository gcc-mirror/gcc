struct Foo(i32, i32, bool);

fn main() {
    let a = Foo(1, 2, true);
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
}
