#[lang = "sized"]
trait Sized {}

struct Foo<const N: usize>;

impl<const N: usize> Foo<N> {
    const VALUE: usize = N;
    const SQUARE: usize = N * N;
}

fn main() -> i32 {
    let a = Foo::<5>::VALUE; // 5
    let b = Foo::<5>::SQUARE; // 25
    (a + b) as i32 - 30
}
