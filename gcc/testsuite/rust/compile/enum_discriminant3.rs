const x: isize = 1;
// { dg-warning "unused name" "" { target *-*-* } .-1 }

fn main() {
    enum Foo {
        Bar = x,
    }
}
