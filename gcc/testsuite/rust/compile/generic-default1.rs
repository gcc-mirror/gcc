struct Foo<A = i321>(A);
// { dg-error "could not resolve type path .i321." "" { target *-*-* } .-1 }

fn main() {
    let a;
    a = Foo(123);
}
