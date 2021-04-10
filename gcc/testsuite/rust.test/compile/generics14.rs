struct Foo<A> {
    a: A,
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
}
