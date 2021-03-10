struct Foo<T> {
    a: T,
    b: bool,
}

fn test<T>(a: T) -> Foo<T> {
    Foo { a: a, b: true }
}

fn main() {
    let a: Foo<i32> = test(123);
    let b: Foo<u32> = test(456);
}
