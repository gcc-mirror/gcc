struct Foo<A = i321>(A);
// { dg-error "failed to resolve TypePath: i321" "" { target *-*-* } .-1 }

fn main() {
    let a;
    a = Foo(123);
}
