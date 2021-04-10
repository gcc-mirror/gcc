struct Foo<A> {
    a: A,
}

struct GenericStruct<T> {
    a: T,
    b: usize,
}

impl Foo<isize> {
    fn test() -> i32 {
        123
    }

    fn bar(self) -> isize {
        self.a
    }
}

fn main() {
    let a: i32 = Foo::test();

    let a2: GenericStruct<i8>;
    a2 = GenericStruct::<i8> { a: 1, b: 456 };

    let b2: i8 = a2.a;
    let c2: usize = a2.b;

    let a4;
    a4 = GenericStruct { a: 1.0, b: 456 };

    let b4: f32 = a4.a;
    let c4: usize = a4.b;
}
