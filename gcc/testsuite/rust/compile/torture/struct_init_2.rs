struct Foo(f32, f32);

fn main() {
    let a = Foo { 0: 10.0, 1: 20.0 };
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
}
