struct Foo<A, B = (A, B)>(A, B);
// { dg-error "type parameters with a default cannot use forward declared identifiers" "" { target *-*-* } .-1 }

fn main() {
    let a: Foo<bool>;
    a = Foo::<bool>(true, (false, true));

    let b: (bool, bool);
    b = a.1;
}
