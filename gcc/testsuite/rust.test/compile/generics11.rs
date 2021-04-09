struct Foo<T>(T, u32);

type TypeAlias = Foo<i32>;

fn main() {
    let a: Foo<i32>;
    a = TypeAlias { 0: 123, 1: 456 };
}
