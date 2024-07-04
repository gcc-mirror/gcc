struct Foo(i32, i32, bool);

fn main() {
    let c = Foo(1, 2f32, true);
    // { dg-error "mismatched types, expected .i32. but got .f32." "" { target *-*-* } .-1 }
}
