#[lang = "sized"]
trait Sized {}

struct Foo<const N: usize>;

impl<const N: usize> Foo<N> {
    const VALUE: usize = N;
}

fn main() -> i32 {
    let val = Foo::<7>::VALUE;
    val as i32 - 7
}
