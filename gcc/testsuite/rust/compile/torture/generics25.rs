struct Foo<A, B = (A, A)>(A, B);

fn main() {
    let a: Foo<bool>;
    a = Foo::<bool>(true, (false, true));

    let b: (bool, bool);
    b = a.1;
}
