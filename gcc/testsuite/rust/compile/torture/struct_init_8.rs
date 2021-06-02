struct Foo(f32, i32);

fn main() {
    let a = Foo { 1: 1, 0: 2f32 };
    let b = Foo { ..a };
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
}
