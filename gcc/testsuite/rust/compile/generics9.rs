struct Foo<A, B = (A, B)>(A, B);
// { dg-error "could not resolve type path .B." "" { target *-*-* } .-1 }

fn main() {
    let a: Foo<bool>;
    a = Foo::<bool>(true, (false, true));

    let b: (bool, bool);
    b = a.1;
}
